StenoStack {

	var busIndices;

	var dryReadIndex = 0, readIndex = 0, writeIndex = 0, through = 0, effectiveSynthIndex = 0, <argumentIndex;
	var nestingDepth = 0, argStack;
	var tokenIndices, tokenIndex;

	*new { |busIndices|
		^super.newCopyArgs(busIndices)
	}

	init {
		tokenIndices = ();
	}

	push {
		// save current args on stack
		argStack = argStack.add([readIndex, writeIndex, readIndex, through, argumentIndex]);
		nestingDepth = nestingDepth + 1;
	}

	pop {
		// set args for subsequent synths
		if(nestingDepth < 1) { Error("inconsistent syntax. This should not happen").throw };
		#readIndex, writeIndex, dryReadIndex, through, argumentIndex = argStack.pop;
		nestingDepth = nestingDepth - 1;
	}

	beginSerial {
		var args;

		this.push;
		argumentIndex = nil;

		args = this.getBusArgs(readIndex, writeIndex + 1, readIndex, through, argumentIndex);

		// set args for subsequent synths
		dryReadIndex = readIndex;
		readIndex = writeIndex = writeIndex + 1;
		through = 0.0;
		^args
	}

	endSerial {
		var args;
		// save current write index
		var previousWriteIndex = writeIndex;

		this.pop;

		args = this.getBusArgs(previousWriteIndex, writeIndex, dryReadIndex, through, argumentIndex);
		// if we are in an operator, count up, because result will be one of the operands
		if(argumentIndex.notNil) { argumentIndex = argumentIndex + 1 };
		^args
	}

	beginParallel {

		this.push;

		argumentIndex = nil;

		// set args for subsequent synths
		dryReadIndex = readIndex;
		readIndex = readIndex; // same same
		writeIndex = writeIndex + 1;
		through = 1.0;
		^[] // nothing needed (dummy synth)
	}

	endParallel {
		var args;
		// save current write index
		var previousWriteIndex = writeIndex;

		this.pop;

		args = this.getBusArgs(previousWriteIndex, writeIndex, dryReadIndex, through, argumentIndex);

		// if we are in an operator, count up, because result will be one of the operands
		if(argumentIndex.notNil) { argumentIndex = argumentIndex + 1 };
		^args

	}

	beginStack {
		var args = this.beginParallel;
		// nary operators
		through = argumentIndex = 0;
		^args
	}

	endStack {
		var readFrom = writeIndex + argumentIndex - 1; // sure?

		this.pop;

		^this.getBusArgs(readFrom, writeIndex, dryReadIndex, through, argumentIndex)
	}

	inOperatorStack {
		^argumentIndex.notNil
	}

	pushOperator { |arity|
		var args;
		argumentIndex = max(0, argumentIndex - arity);
		// args for this synth: in this case: read from the last argument index.
		args = this.getBusArgs(writeIndex + argumentIndex, writeIndex, dryReadIndex, through, argumentIndex);
		// if we are in an operator, count up, next token will represent the next argument
		if(argumentIndex.notNil) { argumentIndex = argumentIndex + 1 };
		^args
	}

	pushLetter {
		var args;
		args = this.getBusArgs(readIndex, writeIndex, dryReadIndex, through, argumentIndex);
		// if we are in an operator, count up, next token will represent the next argument
		if(argumentIndex.notNil) { argumentIndex = argumentIndex + 1 };
		^args
	}

	updateControls { |token|
		// generate some extra information that is passed as arguments to the next synth
		effectiveSynthIndex = effectiveSynthIndex + 1; // only count up for normal synths, not for brackets
		tokenIndices[token] = tokenIndex = if(tokenIndices[token].isNil) { 0 } { tokenIndices[token] + 1 };

	}

	controls {
		^(
			synthIndex: effectiveSynthIndex,
			nestingDepth: nestingDepth,
			tokenIndex: tokenIndex
		)
	}

	// generate synth arguments for in-out-mapping

	getBusArgs { |readIndex, writeIndex, dryReadIndex, through, argumentIndex|
		var readBus = busIndices.clipAt(readIndex);
		var writeBus = busIndices.clipAt(writeIndex + (argumentIndex ? 0));
		var dryReadBus = busIndices.clipAt(dryReadIndex);
		^[\in, readBus, \out, writeBus, \dryIn, dryReadBus, \through, through]
	}


}