
StenoSignal {
	var <inBus, <outBus, <numChannels, <multiChannelExpand;
	var <env, <gate, <fadeTime, <mix, <through, <dryIn;
	var <synthIndex, <depth, <input;
	var outputSignals;

	*new { |inBus, outBus, numChannels, multiChannelExpand = false|
		if(UGen.buildSynthDef.isNil) { "steno signal only works inside a ugen graph".warn };
		^super.newCopyArgs(inBus, outBus, numChannels, multiChannelExpand).init
	}

	init {
		gate = \gate.kr(1);
		fadeTime = \fadeTime.kr(0.02);
		env = EnvGen.kr(Env.asr(0, 1, fadeTime), gate);
		mix = \mix.kr(1);
		through = \through.kr(0);
		dryIn = \dryIn.kr(0);
		synthIndex = \synthIndex.kr(0);
		depth = \nestingDepth.kr(0);
		input = In.ar(inBus, numChannels).asArray;
	}

	// get filter input
	filterInput { |argNumChannels|
		var sig = input.drop(this.offset);
		if(argNumChannels.notNil) {
			sig = sig.keep(argNumChannels);
			if(multiChannelExpand) { sig = sig.wrapExtend(argNumChannels) };
		};
		^sig * env
	}

	// set filter output
	filterOutput { |signal, argNumChannels|
		var gateHappened, detectSignal, oldSignal, drySignal, offset;
		offset = this.offset;
		argNumChannels = min(argNumChannels  ? numChannels, numChannels - offset); // avoid overrun of total channels given

		signal = signal.asArray.keep(argNumChannels);
		if(multiChannelExpand) { signal = signal.wrapExtend(argNumChannels) };
		detectSignal = (gate * 100) + LeakDC.ar(signal.sum); // free the synth only if gate is 0.
		oldSignal = In.ar(outBus + offset, numChannels);          // previous signal on bus
		drySignal = In.ar(dryIn + offset, numChannels);        // dry signal (may come from another bus, but mostly is same as in)
		DetectSilence.ar(detectSignal, time: 0.01, doneAction:2); // free the synth when gate = 0 and fx output is silent
		signal = XFade2.ar(drySignal, signal, mix * 2 - 1); // mix in filter output to dry signal.
		signal = signal + (oldSignal * max(through, 1 - env)); // when the gate is switched off (released), let old input through

		// remove hanging notes if necessary:
		gateHappened = gate <= 0;
		FreeSelf.kr(TDelay.kr(gateHappened, max(fadeTime, \hangTime.kr(30))) + (gateHappened * \steno_unhang.tr(0)));
		this.addOutput(signal);
	}

	// get quelle input
	quelleInput { |argNumChannels|
		var sig = input.drop(this.offset);
		if(argNumChannels.notNil) {
			sig = sig.keep(argNumChannels);
			if(multiChannelExpand) { sig = sig.wrapExtend(argNumChannels) };
		};
		^sig
	}

	// set quelle output
	quelleOutput { |signal, argNumChannels|
		var localMix, localInput, offset;
		offset = this.offset;
		argNumChannels = min(argNumChannels ? numChannels, numChannels - offset); // avoid overrun of total channels given

		signal = signal.asArray.keep(argNumChannels);
		if(multiChannelExpand) { signal = signal.wrapExtend(argNumChannels) };
		localInput = input.asArray.drop(offset).keep(argNumChannels);
		signal = signal * (mix * env) + localInput;  // can't use Out here, because in can be different than out
		this.addOutput(signal);
	}

	// unique filter definition
	filter { |func, multiChannelExpand, argNumChannels|
		var inputSignal = this.filterInput;
		var signal = this.valueUGenFunc(func, inputSignal, multiChannelExpand, argNumChannels);
		this.filterOutput(signal);
	}

	// unique quelle definition
	quelle { |func, multiChannelExpand, argNumChannels|
		var inputSignal = this.quelleInput;
		var signal = this.valueUGenFunc(func, inputSignal, multiChannelExpand, argNumChannels);
		this.filterOutput(signal);
	}

	addOutput { |signal|
		signal = signal.asArray;
		if(outputSignals.size + signal.size > numChannels) {
			"too many signals added, ignoring them:".warn;
			outputSignals.postcs;
		};
		outputSignals = outputSignals.addAll(signal);
	}

	offset {
		^min(outputSignals.size, numChannels)
	}

	writeToBus {
		outputSignals !? {
			ReplaceOut.ar(outBus, outputSignals.keep(numChannels))
		}
	}

	valueUGenFunc { |func, inputSignal, multiChannelExpand, argNumChannels|
		var output = func.value(inputSignal, this).asArray;
		var size = output.size;

		if(multiChannelExpand and: { size < argNumChannels }) { // make it once more, this time the right size.
			output = ({ func.value(inputSignal, this) } ! (argNumChannels div: size).max(1)).flatten(1).keep(argNumChannels);
		};
		if(output.isNil) { output = [0.0] };
		output = output.collect { |x| if(x.rate !== \audio) { K2A.ar(x) } { x } };  // convert output rate if necessary
		if(output.size > argNumChannels) {
			output = SplayAz.ar(argNumChannels, output);  // definitely limit number of channels. // here we could also just keep n channels instead?
			"Mapped synth def function channels from % to % channels\n".postf(size, argNumChannels);
		};
		^output
	}


}

