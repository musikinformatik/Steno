/*
todo: import sets from the web
*/


Steno {

	var <numChannels, <expand, <maxBracketDepth, <server, <bus;
	var <>quant, <settings, <globalSettings;
	var <encyclopedia, <operators;
	var <monitor, <diff;
	var <busses, <synthList, <argList, <variables;
	var <>preProcessor, <>preProcess = true, <cmdLine, <rawCmdLine;
	var <>verbosity = 1; // 0, 1, 2.

	var dryReadIndex = 0, readIndex = 0, writeIndex = 0, through = 0, effectiveSynthIndex = 0, argumentIndex;
	var bracketStack, argStack, tokenIndices;

	classvar <>current;


	*new { |numChannels = 2, expand = false, maxBracketDepth = 8, server|
		^super.newCopyArgs(
			numChannels, expand, maxBracketDepth, server ? Server.default
		).init;
	}

	*push { |numChannels = 2, expand = false, maxBracketDepth = 8, server|
		if(current.notNil) { current.pop.clear };
		^this.new(numChannels, expand, maxBracketDepth, server).push
	}

	init {
		synthList = [];
		argList = [];
		settings = ();
		globalSettings = ();
		variables = ();
		operators = ();
		this.initDiff;
		this.initSynthDefs;
		this.rebuildSynthDefs;
		CmdPeriod.add(this);
		preProcessor =  this.class.defaultPreProcessor;
	}

	clear {
		busses.do { |bus| if(bus.index.notNil) { bus.free } };
		busses = nil;
		variables.do { |bus| if(bus.index.notNil) { bus.free } };
		variables = ();
		this.freeAll;
		this.removeSynthDefs;
		CmdPeriod.remove(this);
	}

	push { |pushSyntax = true|
		current = this;
		if(pushSyntax) {
			thisProcess.interpreter.preProcessor = { |string|
				string = string.copy; // make it mutable
				while { string.beginsWith(Char.nl) } { string = string.drop(1) };
				if(string.beginsWith("(") and: string.endsWith(")")) { string = string.drop(1).drop(-1) };
				if(string.beginsWith("--")) { string = "Steno.current.value(\"%\")".format(string.drop(2)) };
				string
			}
		}
	}

	pop { |popSyntax = true|
		if(current === this) { current = nil };
		if(popSyntax) {
			thisProcess.interpreter.preProcessor = nil
		}
	}

	cmdPeriod {
		diff.init;
		synthList = [];
		argList = [];
	}

	set { |name ... keyValuePairs|
		var setting, defName;
		name = name.asSymbol;
		settings[name] = (setting = settings[name] ?? ());
		setting.putPairs(keyValuePairs);
		defName = this.prefix(name);
		synthList.do { |synth, i|
			if(defName == synth.defName) {
				synth.set(*keyValuePairs)
			}
		};
	}

	setGlobal { |... keyValuePairs|
		globalSettings.putPairs(keyValuePairs);
		synthList.do { |synth| synth.set(*keyValuePairs) };
	}

	numChannels_ { |n|
		numChannels = n;
		this.rebuild;
	}

	expand_ { |flag|
		expand = flag;
		this.rebuild;
	}

	bus_ { |argBus|
		if(argBus.isNumber.not and: { argBus.rate != \audio }) {
			Error("bus must be audio rate and have at least have % channels".format(numChannels)).throw
		};
		bus = argBus;
		this.rebuild;
	}

	rebuild {
		fork {
			this.initBusses;
			this.initSynthDefs;
			this.rebuildSynthDefs;
			server.sync;
			this.resendSynths;
			this.startMonitor(restart: true);
		}
	}

	freeAll {
		diff.init;
		synthList.do(_.release);
		this.releaseHanging;
		synthList = [];
		argList = [];
	}

	releaseHanging {
		if(server.serverRunning) { server.sendMsg("/n_set", 1, \steno_unhang, 1.0) };
	}

	value { |string|
		var processed;
		if(string.isNil) {
			this.resendSynths;
		} {
			processed = string;
			processed !? { processed = processed.stenoStripLineComments };
			if(server.serverRunning.not) { "Server (%) not running.".format(server.name).warn; ^this };
			if(preProcess and: { preProcessor.notNil }) { processed = preProcessor.value(processed, this) };
			string !? { rawCmdLine = string };  // keep unprocessed cmdLine
			string = processed;

			cmdLine = string;
			this.sched({
				server.openBundle;
				protect {
					if(string[0] == $!) {
						this.freeAll;
						string = string.drop(1);
					};
					if(verbosity > 0) { string.postcs };
					this.initArguments; // quick bandaid for an init bug.
					diff.value(string)
				} {
					server.closeBundle(server.latency);
				}
			})
		}
	}

	prevTokens {
		^diff.prevTokens.collect { |x| x.asSymbol }
	}

	resendSynths { |names| // names are symbols
		// we use the one-to-one equivalence of synths and tokens
		this.sched({
			this.prevTokens.do { |token, i|
				var newSynth, args;
				if(names.isNil or: { names.includes(token) }) {
					args = argList.at(i);
					newSynth = this.newSynth(token, i, args);
					if(verbosity > 1) { ("replaced synth" + token).postln };
					synthList.at(i).release;
					synthList.put(i, newSynth);
				}
			}
		})
	}


	startMonitor { |restart = false|

		if(monitor.isPlaying) {
			if(restart) { monitor.release } { ^this }
		};
		monitor = Synth(this.prefix(\monitor),
			[\out, bus ? 0, \in, busses.first, \amp, 0.1],
			addAction:\addAfter
		).register;

	}

	initBusses {
		if(busses.isNil or: { busses.first.numChannels != numChannels }) {
			busses = {
				var bus = Bus.audio(Server.default, numChannels);
				if(bus.isNil) { "not enough busses available!".warn };
				bus
			} ! maxBracketDepth
		};
	}
	//////////////////// getting information about the resulting synth graph ////////////

	dumpStructure { |postDryIn = false|
		var header = String.fill(maxBracketDepth + 4, $-);
		var findBus = { |bus| /*busses.indexOf(bus)*/ bus };
		header = [header, "  %  ", header, "\n"].join;
		argList.do { |args, i|
			var in, out, dryIn;
			header.postf(this.removePrefix(synthList.at(i).defName));

			if(args.isEmpty.not) {
				in = findBus.(args[1]);
				out = findBus.(args[3]);
				dryIn = findBus.(args[5]);

				in.do { "   ".post }; in.postln;
				if(postDryIn) { dryIn.do { "   ".post }; "(%)\n".postf(dryIn) };
				out.do { "   ".post }; out.postln;
			}
		}
	}

	// this represents the structure of the actual implementation
	// not complete

	plotStructure { |title = "untitled"|
		var window, run = true;
		var in, out, prevNode, curNode;
		window = Window(title);
		window.background = Color.black;
		window.onClose = { run = false };
		window.drawFunc = {
			var prevNodes = Array.newClear(busses.size);
			synthList.do { |synth, i|
				var nodeSize = 20;
				var args = argList.at(i);
				var name = this.removePrefix(synth.defName);
				var mul = Point(
					window.bounds.width - (nodeSize * 2) / busses.size,
					window.bounds.height - (nodeSize * 2) / synthList.size
				);
				if(args.isEmpty.not and: { name != ')' }) {
					in = busses.indexOf(args[1]);
					//in = busses.indexOf(args[5]);
					out = busses.indexOf(args[3]);
				} {
					out = out ? 0;
					in = in ? in;
				};
				prevNode = prevNodes[in];
				curNode = Point(out + 1, i + 1) * mul;
				prevNodes[out] = curNode;
				Pen.color = Color.white;
				Pen.moveTo(curNode);
				Pen.addOval(Rect.aboutPoint(curNode, nodeSize, nodeSize));
				Pen.moveTo(curNode);
				if(name != '?') { Pen.stringAtPoint(name, curNode, Font.sansSerif(nodeSize)) };
				Pen.moveTo(curNode);
				prevNode !? { Pen.lineTo(prevNode) };
			};
			Pen.stroke;
		};
		{ while { run } { window.refresh; 0.05.wait; } }.fork(AppClock)
		^window.front;
	}

	dotStructure { |title|
		^this.cmdLine.stenoDotStructure(title, this.maxBracketDepth, variables.keys)
	}


	///////////////// wrappers for UGen functions ///////////////////////

	// external interface

	filter { |name, func, multiChannelExpand, update = true, numChannels|
		numChannels = min(numChannels ? this.numChannels, this.numChannels);
		this.addSynthDef(name, {
			var stenoSignal = StenoSignal(numChannels);
			stenoSignal.filter(func, multiChannelExpand ? expand, numChannels);
			stenoSignal.writeToBus;
			if(verbosity > 0) { ("new filter: \"%\" with % channels\n").postf(name, numChannels) };
		}, update);
	}

	quelle { |name, func, multiChannelExpand, update = true, numChannels|

		numChannels = min(numChannels ? this.numChannels, this.numChannels);
		this.addSynthDef(name, {
			var stenoSignal = StenoSignal(numChannels);
			stenoSignal.quelle(func, multiChannelExpand ? expand, numChannels);
			stenoSignal.writeToBus;
			if(verbosity > 0) { ("new quelle: \"%\" with % channels\n").postf(name, numChannels) };
		}, update);
	}

	// TODO: shapes etc.
	struktur { |name, func, multiChannelExpand, update = true, numChannels|

		numChannels = min(numChannels ? this.numChannels, this.numChannels);
		this.addSynthDef(name, {
			var stenoSignal = StenoSignal(numChannels, multiChannelExpand);
			func.value(stenoSignal.input, stenoSignal);
			stenoSignal.writeToBus;
			if(verbosity > 0) { ("new struktur: \"%\" with % channels\n").postf(name, numChannels) };
		}, update);
	}

	operator { |name, func, arity = 2, multiChannelExpand, update = true|
		var numChannels = this.numChannels;
		var totalNumChannels = numChannels * arity;
		var updateSubgraph;

		updateSubgraph = this.prevTokens.includes(name) and: { operators[name] != arity };

		this.addSynthDef(name, {
			var stenoSignal = StenoSignal(totalNumChannels);
			var inputs = { |i|
				stenoSignal.filterInput(numChannels, i * numChannels);
			} ! arity;
			var outputs = func.value(*inputs.keep(arity)).asArray.keep(numChannels); // todo pass controls.

			stenoSignal.filterOutput(outputs, numChannels);
			stenoSignal.writeToBus;

			if(verbosity > 0) { ("new operator: \"%\" with % channels and arity %\n").postf(name, numChannels, arity) };
		}, update, updateSubgraph);

		operators[name.asSymbol] = arity;
	}


	declareVariables { |names|
		names.do { |name|
			name = name.asSymbol;
			if(variables[name].isNil) {

				this.filter(name, { |input, controls|
					var bus = Bus.audio(server, numChannels);
					var in = XFade2.ar(In.ar(bus, numChannels), InFeedback.ar(bus, numChannels), \feedback.kr.linlin(0, 1, -1, 1));
					variables[name].free; variables[name] = bus;
					Out.ar(bus, input * (\tokenIndex.kr < \assignment.kr(1))); // \assignment can be increased for feeding in more than one signals
					in * controls[\env] + input
				})

			} {
				"Variable '%' already declared".format(name).warn;
			}
		}
	}

	///////////////////////////////////
	// private implementation
	//////////////////////////////////


	// building synth defs

	initSynthDefs {
		// we always go through a limiter here.
		this.addSynthDef(\monitor, { |out, in, amp = 0.1|
			Out.ar(out,
				Limiter.ar(
					In.ar(in, numChannels),
					amp,
					0.1
				)
			)
		});

		this.addSynthDef('(', { |in, out, dryIn, mix = 0, through = 0|
			var feedbackIn = InFeedback.ar(in, numChannels); // out: bus inside parenthesis
			//DelayL.ar(InFeedback.ar(in, numChannels), Rand(0.1), 0.1);
			var drySignal = In.ar(dryIn, numChannels); // dryIn: bus outside parenthesis
			var oldSignal = In.ar(out, numChannels);
			var output = XFade2.ar(drySignal, feedbackIn + (through * oldSignal), mix * 2 - 1);
			XOut.ar(out, EnvGate.new, output);
			// here is a problem, actually we can't reuse a bus that is used for feedback (unless: we want to mix in several feedbacks).
			ReplaceOut.ar(in, Silent.ar(numChannels)); // clean up: overwrite channel with zero.
		});

		this.addSynthDef(')', { |in, out, dryIn, mix = 1, through = 0| // mix = 1: don't add outside in twice
			var drySignal = In.ar(in, numChannels); // out: bus inside parenthesis
			var oldSignal = In.ar(out, numChannels);
			var inputOutside = In.ar(dryIn, numChannels);  // dryIn: bus outside parenthesis
			var output = XFade2.ar(inputOutside, drySignal + (through * oldSignal), mix * 2 - 1);
			XOut.ar(out, EnvGate.new, output); // overwrite the out channel with the new mix
		});

		this.addSynthDef('[', { |in, out|
			ReplaceOut.ar(out, Silent.ar(numChannels)); // umbrella
			FreeSelf.kr(\gate.kr(1) < 1); // dummy synth, can be released
		});

		this.addSynthDef(']', { |in, out, dryIn, mix = 1, through = 0| // mix = 1: don't add outside in twice
			var input = In.ar(in, numChannels);  // in: bus outside parenthesis
			var oldSignal = In.ar(out, numChannels);
			var inputOutside = In.ar(dryIn, numChannels);  // dryIn: bus outside parenthesis
			var output = XFade2.ar(inputOutside, input + (through * oldSignal), mix * 2 - 1);
			XOut.ar(out, EnvGate.new, output);
			ReplaceOut.ar(in, Silent.ar(numChannels)); // clean up bus: overwrite channels with zero, so it can be reused further down
		});


		this.addSynthDef('{', { |in, out|
			FreeSelf.kr(\gate.kr(1) < 1); // dummy synth, can be released
		});

		// same as ]
		this.addSynthDef('}', { |in, out, dryIn, mix = 1, through = 0| // mix = 1: don't add outside in twice
			var input = In.ar(in, numChannels);  // in: bus outside parenthesis
			var oldSignal = In.ar(out, numChannels);
			var inputOutside = In.ar(dryIn, numChannels);  // dryIn: bus outside parenthesis
			var output = XFade2.ar(inputOutside, input + (through * oldSignal), mix * 2 - 1);
			XOut.ar(out, EnvGate.new, output);
			ReplaceOut.ar(in, Silent.ar(numChannels)); // clean up bus: overwrite channels with zero, so it can be reused further down
		});

		this.addSynthDef('?', { FreeSelf.kr(\gate.kr(1) < 1); }); // if not found use this.

	}


	addSynthDef { |name, func, update = true, updateSubgraph = false|
		if(variables.at(name).notNil) { Error("The token '%' is declared as a variable already.".format(name)).throw };
		encyclopedia = encyclopedia ? ();
		encyclopedia.put(name, func);
		SynthDef(this.prefix(name), func).add;
		if(update) {
			fork {
				server.sync;
				if(updateSubgraph) {
					this.resendSynths // for now, just update all
				} {
					this.resendSynths([name])
				}
			}
		}
	}


	rebuildSynthDefs {
		variables = (); // declaration happens in func
		encyclopedia.keysValuesDo { |key, func| SynthDef(this.prefix(key), func).add };
	}

	removeSynthDefs {
		encyclopedia.keysDo { |key|
			var defName = this.prefix(key);
			SynthDescLib.global.removeAt(defName);
			server.sendMsg("/d_free", defName)
		};
	}

	prefix { |key|
		^format("%_%", key, this.identityHash).asSymbol
	}

	removePrefix { |key|
		^key.asString.split($_).at(0).asSymbol
	}

	//  specify the diff algorithm

	initDiff {
		diff = DiffString(
			insertFunc: { |token, i|
				var args = this.calcNextArguments(token, i);
				var synth = this.newSynth(token, i, args);
				synthList = synthList.insert(i, synth);
				argList = argList.insert(i, args);
			},
			removeFunc: { |token, i|
				if(i >= synthList.size) {
					"removeFunc: some inconsistency appeared, nothing to see here, keep going ...".warn;
				} {
					synthList.removeAt(i).release;
					argList.removeAt(i);
				};
			},
			swapFunc: { |token, i|
				var synth, args;
				if(i >= synthList.size) {
					"swapFunc: some inconsistency happened, nothing to see here, keep going ...".warn;
				} {
					args = this.calcNextArguments(token, i);
					synthList.at(i).release;
					synth = this.newSynth(token, i, args);
					synthList.put(i, synth);
					argList.put(i, args);
				};
			},
			keepFunc: { |token, i|
				var args;
				if(i >= synthList.size) {
					"keepFunc: some inconsistency happened, nothing to see here, keep going ...".warn;
				} {
					args = this.calcNextArguments(token, i);
					synthList.at(i).set(*args);
					argList.put(i, args);
				};
			},
			beginFunc: {
				if(Server.default.serverRunning.not) { Error("server not running").throw };
				this.initArguments;
				// add a limiter to the end of the signal chain
				this.startMonitor;
			},
			returnFunc: {
				if(verbosity > 1) { this.dumpStructure };
				this.initArguments; // just to make sure: sometimes it seems that beginFunc isn't called.
			}
		)
	}



	// schedule relative to a time grid
	sched { |func|
		var clock = TempoClock.default;
		if(quant.isNil) { func.value } {
			clock.schedAbs(
				clock.nextTimeOnGrid(quant),
				{ func.value; nil }
			)
		};
	}

	///////////////////////////////////
	// create new synth from token
	///////////////////////////////////

	newSynth { |token, i, args|
		var target = synthList[i - 1];
		var setting;
		token = token.asSymbol;
		if(encyclopedia.at(token).isNil) { token = '?' }; // silent

		^Synth(
			this.prefix(token),
			args,
			target: target, // use previous synth in list. if nil, this is default group.
			addAction: if(target.isNil) { \addToHead } { \addAfter }
		)
	}

	////////////////////////////////////////////////////////
	// function used to step through the syntactic structure
	////////////////////////////////////////////////////////

	initArguments {
		readIndex = writeIndex = dryReadIndex = through = effectiveSynthIndex = 0;
		argumentIndex = nil; // for nary-op functions
		argStack = [];
		bracketStack = [];
		tokenIndices = ();
		this.initBusses;
	}

	calcNextArguments { |token, i|
		var previousWriteIndex, previousArgumentIndex, args, thisSetting, arity;
		token = token.asSymbol;


		switch(token,
			'(', {
				// save current args on stack
				argStack = argStack.add([readIndex, writeIndex, readIndex, through, argumentIndex]);
				bracketStack = bracketStack.add(token);

				argumentIndex = nil;

				// args for this synth
				args = this.getBusArgs(readIndex, writeIndex + 1, readIndex, through, argumentIndex);

				// set args for subsequent synths
				dryReadIndex = readIndex;
				readIndex = writeIndex = writeIndex + 1;
				through = 0.0;


			},
			')', {

				// save current write index
				previousWriteIndex = writeIndex;

				// set args for subsequent synths
				#readIndex, writeIndex, dryReadIndex, through, argumentIndex = argStack.pop;
				bracketStack.pop;


				// args for this synth
				args = this.getBusArgs(previousWriteIndex, writeIndex, dryReadIndex, through, argumentIndex);
				// if we are in an operator, count up, because result will be one of the operands
				if(argumentIndex.notNil) { argumentIndex = argumentIndex + 1 };

			},

			'[', {
				// save current args on stack
				argStack = argStack.add([readIndex, writeIndex, readIndex, through, argumentIndex]);
				bracketStack = bracketStack.add(token);

				argumentIndex = nil;

				// args for this synth
				args = []; // nothing needed (dummy synth)

				// set args for subsequent synths
				dryReadIndex = readIndex;
				readIndex = readIndex; // same same
				writeIndex = writeIndex + 1;
				through = 1.0;


			},
			']', {
				// save current write index
				previousWriteIndex = writeIndex;

				// set args for subsequent synths
				#readIndex, writeIndex, dryReadIndex, through, argumentIndex = argStack.pop;
				bracketStack.pop;


				// args for this synth
				args = this.getBusArgs(previousWriteIndex, writeIndex, dryReadIndex, through, argumentIndex);

				// if we are in an operator, count up, because result will be one of the operands
				if(argumentIndex.notNil) { argumentIndex = argumentIndex + 1 };


			},
			// same as '[', but add argument index = 0
			'{', {

				// save current args on stack
				argStack = argStack.add([readIndex, writeIndex, readIndex, through, argumentIndex]);
				bracketStack = bracketStack.add(token);

				// args for this synth
				args = []; // nothing needed (dummy synth)

				// set args for subsequent synths
				dryReadIndex = readIndex;
				readIndex = readIndex; // same same
				writeIndex = writeIndex + 1;
				through = 1.0;

				// nary operators
				argumentIndex = 0;

			},

			'}', {

				previousWriteIndex = writeIndex + argumentIndex - 1; // sure?

				// set args for subsequent synths
				#readIndex, writeIndex, dryReadIndex, through, argumentIndex = argStack.pop;

				// args for this synth.
				args = this.getBusArgs(previousWriteIndex, writeIndex, dryReadIndex, through, argumentIndex);
			},
			// default case
			{
				arity = operators[token];

				// escape operators that occur outside a stack context
				if(arity.notNil and: { argumentIndex.isNil }) {
					"Operator '%' used outside a stack. Better we ignore it.".format(token).warn;
					token = '?';
				} {
					// generate the arguments for this synth

					// operator
					if(arity.notNil) {

						argumentIndex = max(0, argumentIndex - arity);
						// args for this synth: in this case: read from the last argument index.
						args = this.getBusArgs(writeIndex + argumentIndex, writeIndex, dryReadIndex, through, argumentIndex);

					} {

						// non-operator
						args = this.getBusArgs(readIndex, writeIndex, dryReadIndex, through, argumentIndex)
					};

					// add extra information
					if(tokenIndices[token].isNil) { tokenIndices[token] = 0 };
					args = args ++ [\synthIndex, effectiveSynthIndex, \nestingDepth, bracketStack.size,
						\tokenIndex, tokenIndices[token]];

					// if we are in an operator, count up, next token will represent the next argument
					if(argumentIndex.notNil) { argumentIndex = argumentIndex + 1 };

					// generate some extra information that is passed as arguments to the next synth
					effectiveSynthIndex = effectiveSynthIndex + 1; // only count up for normal synths, not for brackets
					tokenIndices[token] = tokenIndices[token] + 1;
				}

			}
		);
		"after %,  the argument index is %\n".postf(token, argumentIndex);
		//"% args: %\n".postf(token, args);

		thisSetting = globalSettings.copy ? ();
		settings.at(token) !? { thisSetting.putAll(settings.at(token)) };
		^args ++ thisSetting.asKeyValuePairs

	}

	// generate synth arguments for in-out-mapping

	getBusIndex { |index|
		if(index > busses.size) {
			"graph structure too deep, increase maxBracketDepth".warn;
			^busses.last.index
		} {
			busses[index].index
		}

	}

	getBusArgs { |readIndex, writeIndex, dryReadIndex, through, argumentIndex = (0)|
		var readBus = this.getBusIndex(readIndex);
		var writeBus = this.getBusIndex(writeIndex);
		var dryReadBus = this.getBusIndex(dryReadIndex);
		var argumentOffset = argumentIndex * numChannels;
		writeBus = writeBus + argumentOffset;
		^[\in, readBus, \out, writeBus, \dryIn, dryReadBus, \through, through]
	}


	*defaultPreProcessor {
		^#{ |str, steno|
			var newStr = str.class.new, doResend = false, currentClump = str.class.new, hasGap = false;

			if(str.isNil) {
				str = steno.cmdLine ? str.class.new; doResend = true;
			} {
				if(str[0] == $!) { doResend = true; str = str.drop(1); };
				str = str.replace("\n", " ");
			};

			// bring the string into regular form: if it has a gap on the top level ...
			str = str.doBrackets({ |token, i, scope, outerScope, scopeStack|
				if(token.isSpace and: { scopeStack.isEmpty }) {
					hasGap = true
				};
			}, true, steno.verbosity > 0, steno.maxBracketDepth);
			if(hasGap) { str = "[%]".format(str) }; // ... assume parallel parts

			str.doBrackets({ |char, i, scope, outerScope|
				var fstr;
				if("([".includes(char)) {
					outerScope[\currentClump] = currentClump  ++ char;
					currentClump = "";
				} {
					if(")]".includes(char)) {
						if(currentClump.includes(Char.space)) {
							fstr = if(char == $]) { "(%)" } { "[%]" };
							currentClump = currentClump.split(Char.space).collect { |x|
								if(x.size > 1) { fstr.format(x) } { x }
							}.join
						};
						currentClump = outerScope[\currentClump] ++ currentClump ++ char;
					} {
						currentClump = currentClump ++ char;
					}
				};
			}, false, false); // todo: check if we can avoid double checking below
			newStr = newStr ++ currentClump; // add rest.
			newStr = newStr.replace(" ", "");
			if(doResend) { newStr = "!" ++ newStr };

			newStr
		}
	}



}

///////////////////////////  string method //////////////////////////////////

+ String {

	stenoStripLineComments {
		^this.split(Char.nl).collect { |line|
			var i = line.find("//");
			if(i.notNil) { line[..i-1] } { line }
		}.join(Char.nl)
	}


	// we pass to the function: token and index, and then:
	// the current scope inside the last bracket (nil for an opening one)
	// the outer scope when closing a bracket.

	checkBrackets { |fixMistakes = true, warn = true, maxBracketDepth|
		^this.doBrackets(nil, fixMistakes, warn, maxBracketDepth)
	}
}

+ ArrayedCollection {

	doBrackets { |func, fixMistakes = true, warn = true, maxBracketDepth|
		var res, newString = String.new(this.size);
		var pairs = TwoWayIdentityDictionary[
			$( -> $),
			$[ -> $],
			${ -> $},
		];
		var stack = List.new;
		var scopeStack = List.new;
		var scope = (), outerScope = ();
		this.do { |token, i|
			var foundClosing, foundOpening;
			foundOpening = pairs.at(token);
			if(foundOpening.notNil) {
				stack.add(token);
				scopeStack.add(scope);
				outerScope = scope;
				scope = ();
				func.value(token, i, scope, outerScope, scopeStack);
				newString.add(token);
			} {
				foundClosing = pairs.getID(token);
				if(foundClosing.notNil) {
					if(stack.last != foundClosing) {
						if(warn) { ("brackets don't match" + this[0..i+1]).warn };
						if(fixMistakes.not) { ^nil } // otherwise ignore.
					} {
						stack.pop;
						outerScope = scopeStack.pop;
						func.value(token, i, scope, outerScope, scopeStack);
						scope = outerScope;
						newString.add(token);
					}
				} {
					func.value(token, i, scope, outerScope, scopeStack);
					newString.add(token);
				}
			};
			maxBracketDepth !? {
				if(stack.size > maxBracketDepth) {
					("brackets too deep. Increase maxBracketDepth: " + this[0..i+1]).warn;
					^nil
				}
			};

		};
		^if(stack.notEmpty) {
			if(warn) { ("missing closing bracket: ... " + stack).warn };
			if(fixMistakes) {
				stack.reverseDo { |bracket, i|
					var token = pairs.at(bracket);
					newString = newString.add(token);
					func.value(token, i, scopeStack[i]);
				};
				newString
			}
		} {
			newString
		}
	}

	// return dot file for graphviz little language
	// we assume that syntax has already been checked.

	stenoDotStructure { |title = "untitled", attributes = "", variableNames|
		var labelString = "", graphString = "", variableLinks = ();
		this.do { |char, i|
			labelString = labelString ++ format("% [label=\"%\"];\n", i, char);
		};
		this.doBrackets({ |token, i, scope, outerScope|
			if("([{".includes(token)) {
				if(outerScope.notNil) { // on opening a bracket: connect to previous
					scope[\prevNode] = outerScope[\prevNode];
				};
			};
			if("]}".includes(token).not) { // no need to link forward to next bracket
				// print link to previous
				scope[\prevNode] !? {
					graphString = graphString ++ "% -> %;\n".format(scope[\prevNode], i);
				};
			};
			// closing context
			if("])}".includes(token)) {
				// print uplinks in parallel graph
				scope[\upLinks].do { |x|
					graphString = graphString ++ "% -> %;\n".format(x, i);
				};
				// now we can move outside:
				scope = outerScope;
			};

			// if serial, link to previous
			// if parallel, link to top.

			if(scope[\modus] == \parallel) {
				scope[\upLinks] = scope[\upLinks].add(i);
			} {
				scope[\prevNode] = i;
			};

			// switch modus
			if(token == $() { scope[\modus] = \serial };
			if(token == $[) { scope[\modus] = \parallel };
			if(token == ${) { scope[\modus] = \parallel };

			scope // return new current scope for next iteration

		}, false, false);

		if(variableNames.isCollection) {
			this.do { |char, i|
				var name = char.asSymbol;
				if(variableNames.includes(name)) {
					if(variableLinks.at(name).isNil) {
						variableLinks.put(name, i) // define a new variable
					} {
						graphString = graphString ++ "% -> %;\n".format(variableLinks.at(name), i);// ERRROR HERE?
					}
				}
			};
		};
		// ^"digraph %\n{\n%\n%\n}\n}".format(title, attributes, labelString, graphString)
		^"digraph %\n{\n%\n".format(title, attributes) ++ labelString ++ "\n" ++ graphString ++ "}\n}"
	}



}

