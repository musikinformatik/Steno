
StenoSignal {
	var <numChannels, <multiChannelExpand;
	var <inBus, <outBus, <env, <gate, <fadeTime, fadeEnv, <attack, <mix, <through, <dryIn, <feedback;
	var <tailBus; // holds tails of replaced synths
	var <synthIndex, <index, <nestingDepth, <input, <controls;
	var outputSignals;

	*new { |numChannels, multiChannelExpand = false|
		if(UGen.buildSynthDef.isNil) { "steno signal only works inside a ugen graph".warn };
		^super.newCopyArgs(numChannels, multiChannelExpand).init
	}

	init {
		inBus = \in.kr(0);
		dryIn = \dryIn.kr(0);
		input = In.ar(inBus, numChannels).asArray;
		tailBus = \tailBus.ir(0);

		outBus = \out.kr(0);
		outputSignals = Array.fill(numChannels, 0.0);

		gate = \gate.kr(1);
		fadeTime = \fadeTime.kr(0.02);
		attack = \attack.kr(0.02);
		env = EnvGen.kr(Env.asr(attack, 1, fadeTime, [-2, 2]), gate);

		mix = \mix.kr(1);
		through = \through.kr(0); // used for brackets, no external parameter

		synthIndex = \synthIndex.kr(0);
		index = \index.kr(0);
		nestingDepth = \nestingDepth.kr(0);

		feedback = \feedback.kr(0);

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
			feedback: feedback
		);
	}

	// get filter input
	filterInput { |argNumChannels, offset = 0|
		var sig;

		sig = In.ar(inBus, numChannels).asArray.drop(offset);

		if(argNumChannels.notNil) {
			sig = sig.keep(argNumChannels);
			if(multiChannelExpand) { sig = sig.wrapExtend(argNumChannels) };
		};


		^(sig * env)
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
		var gateHappened, dcBlocked, oldSignal, drySignal, tailSignal;
		argNumChannels = min(argNumChannels  ? numChannels, numChannels - offset); // avoid overrun of total channels given

		signal = signal.asArray.keep(argNumChannels);
		if(multiChannelExpand) { signal = signal.wrapExtend(argNumChannels) };

		// gating analysis
		gateHappened = gate <= 0;
		dcBlocked = LeakDC.ar(Sanitize.ar(signal.sum));

		// free synth if signal constant for fadeTime:
		DetectSilence.ar(max(gate, dcBlocked), time: fadeTime, doneAction:2);

		// if signal not constant, remove hanging notes some time after release
		FreeSelf.kr(
			TDelay.kr(gateHappened, max(fadeTime, \hangTime.kr(30)))      //  or
			// + (gateHappened * \steno_unhang.tr(0))
		);


		oldSignal = In.ar(outBus + offset, argNumChannels); // previous signal on bus
		drySignal = In.ar(dryIn  + offset, argNumChannels); // dry signal (mostly same as oldSignal but may come from another bus)
		tailSignal = In.ar(tailBus + offset, argNumChannels); // tails from replaced synths

		// TODO: if replace, set mix to 1

		signal = Mix.ar([
			  // mix filter output with dry signal
			  // signal is supposed to fade out itself (envelope assigned at filter input)
			XFade2.ar(drySignal, signal, MulAdd(mix, 2, -1)),

			  // collect tails
			tailSignal,

			  // fade old signal according to 1-env
			  // `through` is used to carry original signal in the parallel case
			oldSignal * max(through, 1 - env)
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

		FreeSelfWhenDone.kr(env);                                    // free synth if gate 0
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

	writeToBus {
		outputSignals !? {
			outputSignals.keep(numChannels);
			// write to tailBus if going to be replaced, else write silence
			// TODO: write silence if release (not replace)
			ReplaceOut.ar(tailBus, outputSignals.keep(numChannels) * (1 - gate));

			// write to outBus unless goint to be replaced
			// TODO: write to outBus if release (not replace)
			XOut.ar(outBus, gate, outputSignals.keep(numChannels));

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
			output = SplayAz.ar(argNumChannels, output);
			"Mapped synth def function channels from % to % channels\n".postf(size, argNumChannels);
		};
		^output
	}


}

