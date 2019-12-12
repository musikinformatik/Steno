/*

StenoSignal encapsulates the signal routing in steno. It is always created inside a SynthDef
Its counterpart is StenoStack, which sets the parameters for each synth

*/

StenoSignal {
	var <numChannels, <multiChannelExpand;
	var <inBus, <outBus, <tailBus, <mix, <through, <dryIn, <feedback;
	var <env, <gate, <fadeTime, fadeEnv, <attack;
	var <synthIndex, <index, <nestingDepth, <input, <controls;
	var outputSignals;

	*new { |numChannels, multiChannelExpand = false|
		if(UGen.buildSynthDef.isNil) { "steno signal only works inside a ugen graph".warn };
		^super.newCopyArgs(numChannels, multiChannelExpand).init
	}

	init {
		this.initIO;
		this.initEnvelope;
		this.initControls;
	}

	initIO {
		inBus = \in.kr(0); // the bus from which the "in" argument reads
		outBus = \out.kr(0); // the bus onto which the output signal is written
		dryIn = \dryIn.kr(0); // the bus of the previous level of nesting
		tailBus = \tailBus.ir(0); // the bus for the tailing off signals of released synths

		input = In.ar(inBus, numChannels).asArray; // the input passed to the "in" argument
		outputSignals = Array.fill(numChannels, 0.0); // the output channels that the outputs are added to

		// mix is the amount of signal contributed to the output
		mix = \mix.kr(1);

		// through is how much to let through the old signal on the out bus.
		// it is set to 1 for parallel synths, which read from another bus and all mix a signal on their output
		through = \through.kr(0);
	}

	initEnvelope {
		gate = \gate.kr(1); // when gate becomes zero, the synth is released
		fadeTime = \fadeTime.kr(0.02); // the crossfade time
		attack = \attack.kr(0.02); // the attack crossfade time
		env = EnvGen.kr(Env.asr(attack, 1, fadeTime, [-2, 2]), gate); // the crossfading envelope.
	}

	initControls {

		synthIndex = \synthIndex.kr(0); // the number of synths of the same name that have occurred before
		index = \index.kr(0); // the total number of synths, excluding special characters like brackets
		nestingDepth = \nestingDepth.kr(0); // the current depth

		feedback = \feedback.kr(0); // a control used by feedback ugens (variable declarations)

		// the controls are passed as second argument to synth functions so that uges can depend on them
		// see also StenoStack:updateControls
		controls = (
			index: index,
			synthIndex: synthIndex,
			nestingDepth: nestingDepth,
			mix: mix,
			gate: gate,
			numChannels: numChannels,
			through: through,
			env: env,
			fadeTime: fadeTime,
			attack: attack,
			feedback: feedback,
			fb: feedback,
			dpth: nestingDepth,
			idx: index,
			sidx: synthIndex
		);
	}


	// get filter input
	filterInput { |argNumChannels, offset = 0|
		^this.quelleInput(argNumChannels, offset) * env
	}

	// get quelle input
	quelleInput { |argNumChannels, offset = 0|
		var sig = input.drop(offset);
		if(argNumChannels.notNil) {
			sig = sig.keep(argNumChannels);
			if(multiChannelExpand) { sig = sig.wrapExtend(argNumChannels) };
		};

		^sig
	}

	// set filter output
	filterOutput { |signal, argNumChannels, offset = 0|
		var oldSignal, tailSignal, drySignal;
		argNumChannels = min(argNumChannels  ? numChannels, numChannels - offset); // avoid overrun of total channels given

		signal = signal.asArray.keep(argNumChannels);
		if(multiChannelExpand) { signal = signal.wrapExtend(argNumChannels) };

		this.freeSelfWhenSilent(signal);

		oldSignal = In.ar(outBus + offset, argNumChannels); // previous signal on bus
		tailSignal = In.ar(tailBus + offset, argNumChannels); // tails from replaced synths

		signal = Mix.ar([
			// mix filter output with dry signal
			// signal is supposed to fade out itself (envelope assigned at filter input)
			XFade2.ar(oldSignal, signal, MulAdd(mix, 2, -1)),

			// collect tails
			tailSignal,

			// `through` is used to carry original signal in the parallel case
			oldSignal * (through * env)
		]);

		this.addOutput(signal, offset);
	}

	// set quelle output
	quelleOutput { |signal, argNumChannels, offset = 0|
		var oldSignal, tailSignal;
		argNumChannels = min(argNumChannels ? numChannels, numChannels - offset); // avoid overrun of total channels given

		signal = signal.asArray.keep(argNumChannels);
		if(multiChannelExpand) { signal = signal.wrapExtend(argNumChannels) };

		oldSignal  = In.ar(outBus + offset, argNumChannels);  // previous signal on bus
		tailSignal = In.ar(tailBus + offset, argNumChannels); // tails from replaced synths


		signal = Mix.ar([
			// signal strength (mix) and envelope multiplication
			signal * mix * env,

			// collect tails
			tailSignal,

			// only add old signal if last in replaceGroup
			// TODO: if release (not replace), continue mixing
			oldSignal * gate
		]);

		FreeSelfWhenDone.kr(env);    // free synth if gate 0
		this.addOutput(signal, offset);
	}

	// unique filter definition
	filter { |func, multiChannelExpand, argNumChannels|
		var inputSignal = this.filterInput;
		var signal = this.valueUGenFunc(func, inputSignal, multiChannelExpand, argNumChannels);
		if(signal.notNil) {
			this.filterOutput(signal, signal.size);
		}
	}

	// unique quelle definition
	quelle { |func, multiChannelExpand, argNumChannels|
		var inputSignal = this.quelleInput;
		var signal = this.valueUGenFunc(func, inputSignal, multiChannelExpand, argNumChannels);
		if(signal.notNil) {
			this.quelleOutput(signal, signal.size)
		}
	}

	addOutput { |signal, offset = 0|
		signal = signal.asArray;
		if(signal.shape.size > 1) { "wrong signal shape".warn; signal.postcs };
		if(signal.size + offset > numChannels) {
			"too many signals added, ignoring them:".warn;
			outputSignals.postcs;
		};
		signal.do { |x, i|
			i = i + offset;
			outputSignals[i] = outputSignals[i] + x;
		}
	}

	// we try to keep this uniform for all kinds of signals.
	writeToBus {
		outputSignals !? {
			outputSignals = outputSignals.keep(numChannels);

			// write to tailBus if going to be replaced, else write silence
			// TODO (?):
			// if release: write silence -> tailBus
			// if replaced: write output signals -> tailBus
			ReplaceOut.ar(tailBus, outputSignals * (1 - gate));

			// write to outBus unless it is going to be replaced
			// TODO (?):
			// if release: write outputSignals -> outBus
			// if replaced: write silence -> outBus ?

			XOut.ar(outBus, gate, outputSignals);

		}
	}

	valueUGenFunc { |func, inputSignal, multiChannelExpand, argNumChannels|
		var output, size;

		output = func.value(inputSignal, controls);
		if(output.isNil) { ^nil };

		output = output.asArray;
		size = output.size;


		if(multiChannelExpand and: { size < argNumChannels }) { // make it once more, this time the right size.
			output = ({ func.value(inputSignal, controls) } ! (argNumChannels div: size).max(1)).flatten(1).keep(argNumChannels);
		};

		output = output.collect { |x| if(x.rate !== \audio) { K2A.ar(x) } { x } };  // convert output rate if necessary
		if(output.size > argNumChannels) {
			// definitely limit number of channels.
			// here we could also just keep n channels instead?
			// this would happen in writeToBus automatically
			output = SplayAz.ar(argNumChannels, output);
			"Mapped synth def function channels from % to % channels\n".postf(size, argNumChannels);
		};
		^output
	}


	// internal special functions

	// used in filter
	freeSelfWhenSilent { |signal|
		var sumSignal, gateHappened, dcBlocked;
		// gating analysis
		gateHappened = gate <= 0;
		sumSignal = signal.sum;
		if(\Sanitize.asClass.notNil) { sumSignal = Sanitize.ar(sumSignal) };
		dcBlocked = LeakDC.ar(sumSignal);

		// free synth if signal constant for fadeTime:
		DetectSilence.ar(max(gate, dcBlocked), time: fadeTime, doneAction:2);

		// if signal not constant, remove hanging notes some time after release
		FreeSelf.kr(
			TDelay.kr(gateHappened, max(fadeTime, \hangTime.kr(30)))      //  or
			// + (gateHappened * \steno_unhang.tr(0))
		);
	}

	// this happens when we go one nesting level up
	// for parentheses, brackets and operators
	closeBracket {
		var oldSignal, inputOutside, signal;

		// TODO: check if we need outBus + offset for a closing parenthesis inside an operator
		oldSignal = In.ar(outBus, numChannels); // the old signal on the bus, mixed in by through
		inputOutside = In.ar(dryIn, numChannels);  // dryIn: bus outside parenthesis

		signal = XFade2.ar(inputOutside, input, MulAdd(mix, 2, -1)) * env;

		// through controls balance between serial bus result and outside bus
		signal = Mix.ar([
			signal * env,
			oldSignal * (through * env), //max(through, 1 - env)
			inputOutside * ((1 - env) * (1 - through))
		]);

		// fade old input according to gate, signal is supposed to fade out itself.
		FreeSelfWhenDone.kr(env); // free synth if gate 0

		// the last one cleans up - overwrite channel with zero:
		ReplaceOut.ar(inBus, Silent.ar(numChannels));

		this.addOutput(signal);

	}

	// this happens when we enter a serial structure (opening round parenthesis)
	beginSerial {
		var oldSignal, inputOutside, signal;

		// TODO: check if we need outBus + offset for an opening parenthesis inside an operator
		oldSignal = In.ar(outBus, numChannels);  // previous signal on bus
		inputOutside = In.ar(dryIn, numChannels); // dryIn: bus outside parenthesis

		oldSignal = oldSignal * through; // when through is zero (serial in serial mode), play nothing from the same bus.
		signal = XFade2.ar(oldSignal, inputOutside, MulAdd(mix, 2, -1));

		FreeSelfWhenDone.kr(env); // free synth if gate 0

		// for now, we just write output to outBus, not to tailBus.
		// TODO: check if this is ok.

		XOut.ar(outBus, env, signal);
	}


}

