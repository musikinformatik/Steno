StenoStack {

	var busIndices, <controls;
	var argStack;

	var dryReadIndex = 0, readIndex = 0, writeIndex = 0, through = 0, <argumentIndex;
	var nestingDepth = 0;


	*new { |busIndices|
		^super.newCopyArgs(busIndices, (tokenIndices: (), synthIndex: -1))
	}

	push {
		// save current args on stack
		argStack = argStack.add([readIndex, writeIndex, dryReadIndex, through, argumentIndex]);
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

		writeIndex = writeIndex + 1;
		argumentIndex !? { writeIndex = writeIndex + argumentIndex };
		argumentIndex = nil;

		args = this.getBusArgs(readIndex, writeIndex, readIndex, 0, argumentIndex); // no through

		// set args for subsequent synths
		dryReadIndex = readIndex;
		readIndex = writeIndex;
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
		argumentIndex !? { writeIndex = writeIndex + argumentIndex };
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
		through = 0;
		argumentIndex = 0;
		^args
	}

	endStack {
		var args, readFrom;

		readFrom = writeIndex + argumentIndex - 1;

		this.pop;

		args = this.getBusArgs(readFrom, writeIndex, dryReadIndex, through, argumentIndex);

		// if we are in an operator, count up, because result will be one of the operands
		if(argumentIndex.notNil) { argumentIndex = argumentIndex + 1 };

		^args
	}

	inOperatorStack {
		^argumentIndex.notNil
	}

	pushOperator { |token, arity|
		var args;
		argumentIndex = max(0, argumentIndex - arity);
		// args for this synth: in this case: read from the last argument index.
		args = this.getBusArgs(writeIndex + argumentIndex, writeIndex, dryReadIndex, through, argumentIndex);
		this.updateControls(token); // for the next letter
		// if we are in an operator, count up, next token will represent the next argument
		if(argumentIndex.notNil) { argumentIndex = argumentIndex + 1 };
		^args
	}

	pushLetter { |token|
		var args;
		args = this.getBusArgs(readIndex, writeIndex, dryReadIndex, through, argumentIndex);
		this.updateControls(token); // for the next letter
		// if we are in an operator, count up, next token will represent the next argument
		if(argumentIndex.notNil) { argumentIndex = argumentIndex + 1 };
		^args
	}

	updateControls { |token|
		// generate some extra information that is passed as arguments to the next synth
		controls.use {
			var tokenIndex = ~tokenIndices[token];
			~tokenIndices[token] = ~index = if(tokenIndex.isNil) { 0 } { tokenIndex + 1 };
			~token = token;
			~depth = nestingDepth;
			~synthIndex = ~synthIndex + 1; // only count up for normal synths, not for brackets
		}
	}

	// generate synth arguments for in-out-mapping

	getBusArgs { |readIndex, writeIndex, dryReadIndex, through, argumentIndex|
		var readBus = busIndices.clipAt(readIndex);
		var writeBus = busIndices.clipAt(writeIndex + (argumentIndex ? 0));
		var dryReadBus = busIndices.clipAt(dryReadIndex);
		^[\in, readBus, \out, writeBus, \dryIn, dryReadBus, \through, through]
	}


}