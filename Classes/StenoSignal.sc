
StenoSignal {
	var <inBus, <outBus, <numChannels;
	var <env, <gate, <fadeTime, <mix, <through, <dryIn;
	var <synthIndex, <depth, <input;
	var outputSignals;

	*new { |inBus, outBus, numChannels|
		if(UGen.buildSynthDef.isNil) { "steno signal only works inside a ugen graph".warn };
		^super.newCopyArgs(inBus, outBus, numChannels).init
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

	addOutput { |signal|
		signal = signal.asArray;
		if(outputSignals.size + signal.size > numChannels) {
			"too many signals added, ignoring them".warn;
		};
		outputSignals = outputSignals.addAll(signal)
	}

	writeToBus {
		ReplaceOut.ar(outBus, outputSignals.keep(numChannels))
	}

	filterInput { |argNumChannels, offset|
		^if(argNumChannels.notNil) {
			input.drop(offset).keep(argNumChannels) * env
		} {
			input * env
		}
	}

	filterOutput { |signal, argNumChannels, offset|
		var gateHappened;
		var detectSignal = (gate * 100) + LeakDC.ar(signal.asArray.sum); // free the synth only if gate is 0.
		var oldSignal = In.ar(outBus, numChannels);          // previous signal on bus
		var drySignal = In.ar(dryIn, numChannels);        // dry signal (may come from another bus, but mostly is same as in)
		DetectSilence.ar(detectSignal, time: 0.01, doneAction:2); // free the synth when gate = 0 and fx output is silent
		signal = XFade2.ar(drySignal, signal, mix * 2 - 1); // mix in filter output to dry signal.
		signal = signal + (oldSignal * max(through, 1 - env)); // when the gate is switched off (released), let old input through

		// remove hanging notes if necessary:
		gateHappened = gate <= 0;
		FreeSelf.kr(TDelay.kr(gateHappened, max(fadeTime, \hangTime.kr(30))) + (gateHappened * \steno_unhang.tr(0)));
		this.addOutput(signal);
	}

	quelleInput {
		^input
	}

	quelleOutput { |signal|
		signal = signal * (mix * env) + input;  // can't use Out here, because in can be different than out
		this.addOutput(signal);
	}

	filter { |func, multiChannelExpand, argNumChannels|
		var inputSignal = this.filterInput;
		var signal = this.valueUGenFunc(func, inputSignal, multiChannelExpand, argNumChannels);
		this.filterOutput(signal);
	}

	quelle { |func, multiChannelExpand, argNumChannels|
		var inputSignal = this.quelleInput;
		var signal = this.valueUGenFunc(func, inputSignal, multiChannelExpand, argNumChannels);
		this.filterOutput(signal);
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

