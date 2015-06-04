
StenoSignal {
	var <numChannels, <multiChannelExpand;
	var <inBus, <outBus, <env, <gate, <fadeTime, <mix, <through, <dryIn;
	var <synthIndex, <depth, <input, <controls;
	var outputSignals;

	*new { |numChannels, multiChannelExpand = false|
		if(UGen.buildSynthDef.isNil) { "steno signal only works inside a ugen graph".warn };
		^super.newCopyArgs(numChannels, multiChannelExpand).init
	}

	init {
		inBus = \in.kr(0);
		outBus = \out.kr(0);
		gate = \gate.kr(1);
		fadeTime = \fadeTime.kr(0.02);
		env = EnvGen.kr(Env.asr(0, 1, fadeTime), gate);
		mix = \mix.kr(1);
		through = \through.kr(0);
		dryIn = \dryIn.kr(0);
		synthIndex = \synthIndex.kr(0);
		depth = \nestingDepth.kr(0);
		input = In.ar(inBus, numChannels).asArray;
		outputSignals = Array.fill(numChannels, 0.0);
		controls = (
			index: synthIndex,
			depth: depth,
			mix: mix,
			gate: gate,
			numChannels: numChannels,
			through: through,
			env: env
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
		var gateHappened, detectSignal, oldSignal, drySignal;

		argNumChannels = min(argNumChannels  ? numChannels, numChannels - offset); // avoid overrun of total channels given

		signal = signal.asArray.keep(argNumChannels);
		if(multiChannelExpand) { signal = signal.wrapExtend(argNumChannels) };
		detectSignal = (gate * 100) + LeakDC.ar(signal.sum); // free the synth only if gate is 0.
		oldSignal = In.ar(outBus + offset, argNumChannels);          // previous signal on bus
		drySignal = In.ar(dryIn + offset, argNumChannels);        // dry signal (may come from another bus, but mostly is same as in)
		DetectSilence.ar(detectSignal, time: 0.01, doneAction:2); // free the synth when gate = 0 and fx output is silent
		signal = XFade2.ar(drySignal, signal, mix * 2 - 1); // mix in filter output to dry signal.
		signal = signal + (oldSignal * max(through, 1 - env)); // when the gate is switched off (released), let old input through

		// remove hanging notes if necessary:
		gateHappened = gate <= 0;
		FreeSelf.kr(TDelay.kr(gateHappened, max(fadeTime, \hangTime.kr(30))) + (gateHappened * \steno_unhang.tr(0)));
		this.addOutput(signal, offset);
	}

	// set quelle output
	quelleOutput { |signal, argNumChannels, offset = 0|
		var localMix, localInput, oldSignal;
		argNumChannels = min(argNumChannels ? numChannels, numChannels - offset); // avoid overrun of total channels given

		signal = signal.asArray.keep(argNumChannels);
		if(multiChannelExpand) { signal = signal.wrapExtend(argNumChannels) };

		oldSignal = In.ar(outBus + offset, argNumChannels);          // previous signal on bus
		localInput = this.quelleInput(argNumChannels, offset);

		signal = XFade2.ar(oldSignal, signal + oldSignal, (mix * env));  // can't use Out here, because "in" can be different than "out"
		this.addOutput(signal, offset);
	}

	// unique filter definition
	filter { |func, multiChannelExpand, argNumChannels|
		var inputSignal = this.filterInput;
		var signal = this.valueUGenFunc(func, inputSignal, multiChannelExpand, argNumChannels);
		this.filterOutput(signal, signal.size);
	}

	// unique quelle definition
	quelle { |func, multiChannelExpand, argNumChannels|
		var inputSignal = this.quelleInput;
		var signal = this.valueUGenFunc(func, inputSignal, multiChannelExpand, argNumChannels);
		this.quelleOutput(signal, signal.size);
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
			ReplaceOut.ar(outBus, outputSignals.keep(numChannels))
		}
	}

	valueUGenFunc { |func, inputSignal, multiChannelExpand, argNumChannels|

		var output = func.value(inputSignal, controls).asArray;
		var size = output.size;

		if(multiChannelExpand and: { size < argNumChannels }) { // make it once more, this time the right size.
			output = ({ func.value(inputSignal, controls) } ! (argNumChannels div: size).max(1)).flatten(1).keep(argNumChannels);
		};
		if(output.isNil) { output = [0.0] };
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

