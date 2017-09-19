
StenoSignal {
	var <numChannels, <multiChannelExpand;
	// LFSaw.de: made attack an equitable param
	var <inBus, <outBus, <env, <gate, <fadeTime, fadeEnv, <attack, <mix, <through, <dryIn;
	var <fadeBus, replacement; // LFSaw.de: used for filter fades
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
		fadeBus = \fadeBus.ir(0); // LFSaw.de: used for filter fades
		replacement = \replacement.ir(0);

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
			attack: attack
		);
	}

	// get filter input
	filterInput { |argNumChannels, offset = 0|
		var sig, bus;
		// if this synth is a replacement for another one, 
		// read from alternative bus during attack time, switch to inBus afterwards
		bus = Select.kr(replacement * (1-ToggleFF.kr(env < 1)), [inBus, fadeBus]);
		sig = In.ar(bus, numChannels).asArray.drop(offset);

		if(argNumChannels.notNil) {
			sig = sig.keep(argNumChannels);
			if(multiChannelExpand) { sig = sig.wrapExtend(argNumChannels) };
		};

		// if gate is released, write unprocessed input to fadeBus
		ReplaceOut.ar(fadeBus, Select.ar(ToggleFF.kr(gate), [input, Silent.ar()]));


		^(sig * env)
	}

	// get quelle input
	quelleInput { |argNumChannels, offset = 0|
		var sig = input.drop(offset);
		if(argNumChannels.notNil) {
			sig = sig.keep(argNumChannels);
			if(multiChannelExpand) { sig = sig.wrapExtend(argNumChannels) };
		};

		// if gate is released, write unprocessed input to fadeBus
		ReplaceOut.ar(fadeBus, Select.ar(ToggleFF.kr(gate), [input, Silent.ar()]));

		^sig
	}

	// set filter output
	filterOutput { |signal, argNumChannels, offset = 0|
		var gateHappened, dcBlocked, oldSignal, drySignal;
		argNumChannels = min(argNumChannels  ? numChannels, numChannels - offset); // avoid overrun of total channels given

		signal = signal.asArray.keep(argNumChannels);
		if(multiChannelExpand) { signal = signal.wrapExtend(argNumChannels) };

		// LFSaw.de: grouped elements thematically, added comments, changed var names, fixed fading

		// gating analysis
		gateHappened = gate <= 0;
		dcBlocked = LeakDC.ar(signal.sum);

		// free synth if signal constant for fadeTime:
		DetectSilence.ar(max(gate, dcBlocked), time: fadeTime, doneAction:2);

		// if signal not constant, remove hanging notes some time after release
		FreeSelf.kr(
			TDelay.kr(gateHappened, max(fadeTime, \hangTime.kr(30)))      //  or
			// + (gateHappened * \steno_unhang.tr(0))
		);


		oldSignal = In.ar(outBus + offset, argNumChannels); // previous signal on bus
		drySignal = In.ar(dryIn  + offset, argNumChannels); // dry signal (mostly same as oldSignal but may come from another bus)

		signal = XFade2.ar(drySignal, signal, MulAdd(mix, 2, -1)); // mix filter output with dry signal
		signal = Mix.ar([signal, oldSignal * max(through.varlag(fadeTime, start: 0), 1 - env)]);   // fade old input according to gate, signal is supposed to fade out itself.

		this.addOutput(signal, offset);
	}

	// set quelle output
	quelleOutput { |signal, argNumChannels, offset = 0|
		var localMix, oldSignal;
		argNumChannels = min(argNumChannels ? numChannels, numChannels - offset); // avoid overrun of total channels given

		signal = signal.asArray.keep(argNumChannels);
		if(multiChannelExpand) { signal = signal.wrapExtend(argNumChannels) };

		// LFSaw.de: reordered to group thematical elements together
		oldSignal = In.ar(outBus + offset, argNumChannels);          // previous signal on bus

		signal = XFade2.ar(oldSignal, MulAdd(signal, env, oldSignal), MulAdd(mix, 2, -1));

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
			ReplaceOut.ar(outBus.varlag(
				fadeTime, 
				warp: \hold)
			, outputSignals.keep(numChannels))
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

