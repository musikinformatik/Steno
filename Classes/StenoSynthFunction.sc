/*

todo:

- check the relation between signalNumChannels and numChannels
- check if multiChannelExpand interface could be unified.

*/

AbstractStenoSynthFunction {

	var <func, <numChannels;
	var <>multiChannelExpand;
	var <stenoSignal;

	*new { |func, numChannels, multiChannelExpand|
		^this.newCopyArgs(func, numChannels, multiChannelExpand ? false)
	}

	value {
		^this.subclassResponsibility(thisMethod)
	}
}


StenoFilterFunction : AbstractStenoSynthFunction {

	value {
		stenoSignal = StenoSignal(numChannels);
		stenoSignal.filter(func, multiChannelExpand, numChannels);
		stenoSignal.writeToBus;
	}

}


StenoQuelleFunction : AbstractStenoSynthFunction {

	value {
		stenoSignal = StenoSignal(numChannels);
		stenoSignal.quelle(func, multiChannelExpand, numChannels);
		stenoSignal.writeToBus;
	}

}

StenoStrukturFunction : AbstractStenoSynthFunction {

	value {
		stenoSignal = StenoSignal(numChannels, multiChannelExpand);
		// pass the signal object, so func can use it, e.g. by calling filter tor quelle in combinations
		func.value(stenoSignal.input, stenoSignal);
		stenoSignal.writeToBus;
	}

}


StenoOperatorFunction : AbstractStenoSynthFunction {

	var <>arity, <totalNumChannels;

	value {
		var inputs, outputs;
		totalNumChannels = numChannels * arity;
		stenoSignal = StenoSignal(totalNumChannels);
		inputs = { |i|
			stenoSignal.filterInput(numChannels, i * numChannels);
		} ! arity;

		outputs = func.value(*inputs.keep(arity).add(stenoSignal.controls));
		if(outputs.notNil) {
			outputs = outputs.asArray.keep(numChannels);
			stenoSignal.filterOutput(outputs, numChannels);
			stenoSignal.writeToBus;
		};
	}

}

StenoClosingParenthesis : AbstractStenoSynthFunction {

	value {
		var oldSignal, inputOutside, stenoSignal, signal, signalOnMixBus;

		stenoSignal = StenoSignal(numChannels);
		stenoSignal.filterInput;
		oldSignal = In.ar(stenoSignal.outBus, numChannels); // the old signal on the bus, mixed in by through
		inputOutside = In.ar(stenoSignal.dryIn, numChannels);  // dryIn: bus outside parenthesis

		signal = XFade2.ar(inputOutside, stenoSignal.input, MulAdd(stenoSignal.mix, 2, -1));

		signal = Mix.ar([
			signal,
			oldSignal * max(
				stenoSignal.through,
				1 - stenoSignal.env
			)
		]);   // fade old input according to gate, signal is supposed to fade out itself.

		FreeSelfWhenDone.kr(stenoSignal.env); // free synth if gate 0

		stenoSignal.addOutput(signal);
		stenoSignal.writeToBus;

	}

}


StenoOpeningParenthesis : AbstractStenoSynthFunction {

	// writes nothing.
	value {
		stenoSignal = StenoSignal(numChannels);
		FreeSelfWhenDone.kr(stenoSignal.env); // free synth when envelope has ended (maybe also clould free on gate = 0)
	}

}
