
Steno {

	var <numChannels, <expand, <maxBracketDepth, <server, <bus;
	var <>quant, <settings;
	var <encyclopedia, <operators;
	var <monitor, <>diff;
	var <busIndices, <>synthList, <>argList, <variables;
	var <tailBus;
	var <>preProcessor, <>preProcess = true, <cmdLine, <rawCmdLine;
	var <>verbosity = 1; // 0, 1, 2.
	var <group;

	var <argumentStack;
	var <>beginFunction;
	var ignoreReplaceAll = false;

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
		if(server.serverRunning.not) {
			Error("server % not running".format(server)).throw
		};
		synthList = [];
		argList = [];
		variables = ();
		operators = ();
		settings = StenoSettings.new;
		this.initBusses;
		this.initDiff;
		this.initSynthDefs;
		this.rebuildSynthDefs;
		CmdPeriod.add(this);
		preProcessor =  this.class.defaultPreProcessor;
	}

	clear {
		busIndices !? {
			server.audioBusAllocator.free(busIndices.first);
			busIndices = nil;
		};
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
				while { string.beginsWith("\n") } { string = string.drop(1) };
				if(string.beginsWith("(\n--") and: string.endsWith("\n)")) { string = string.drop(2).drop(-2) };
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
		group = nil;
	}

	set { |name ... keyValuePairs|
		this.sched {
			settings.set(name, keyValuePairs);
			this.reeval;
		}
	}

	setGlobal { |... keyValuePairs|
		this.sched {
			settings.setGlobal(keyValuePairs);
			this.reeval;
		}
	}

	get { |name, key|
		^settings.get(name, key)
	}

	getGlobal { |key|
		^settings.getGlobal(key)
	}

	fadeTime_ { |dt|
		this.setGlobal(\fadeTime, dt)
	}

	fadeTime {
		^this.getGlobal(\fadeTime)
	}

	numChannels_ { |n|
		if(n != numChannels) {
			busIndices = nil;
			numChannels = n;
			this.rebuildGraph;
		};
	}

	expand_ { |flag|
		expand = flag;
		this.rebuildGraph;
	}

	bus_ { |argBus|
		if(argBus.isNumber.not and: { argBus.rate != \audio }) {
			Error("bus must be audio rate and have at least % channels".format(numChannels)).throw
		};
		bus = argBus;
		this.rebuild;
	}

	rebuild {
		fork {
			this.initBusses;
			this.rebuildSynthDefs;
			server.sync;
			this.sched {
				this.startGroup;
				this.resendSynths;
				this.startMonitor(restart: true);
			}
		}
	}

	rebuildGraph {
		fork {
			this.freeAll;
			this.initBusses;
			this.rebuildSynthDefs;
			server.sync;
			this.startGroup;
			this.value(rawCmdLine);
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
		if(server.serverRunning and: { group.notNil }) { server.sendMsg("/n_set", group.nodeID, \steno_unhang, 1.0) };
	}

	value { |string|
		var processed;
		this.sched {
			if(string.isNil) {
				this.resendSynths;
			} {
				processed = string;
				processed !? { processed = processed.stenoStripLineComments };
				if(server.serverRunning.not) { "Server (%) not running.".format(server.name).warn; ^this };
				if(preProcess and: { preProcessor.notNil }) { processed = preProcessor.value(processed, this) };
				string !? { rawCmdLine = string };  // keep unprocessed cmdLine
				this.eval(processed)
			}
		}
	}


	eval { |string|
		string = string ? "";
		cmdLine = string;

		server.openBundle;
		protect {
			if(verbosity > 0) { string.postcs };
			diff.value(string)
		} {
			server.closeBundle(server.latency);
		}

	}

	reeval {
		ignoreReplaceAll = true;
		this.eval(cmdLine);
		ignoreReplaceAll = false;
	}

	prevTokens {
		^diff.prevTokens.collect { |x| x.asSymbol }
	}

	resendSynths { |names| // names are symbols
		var oldSynthList = synthList.copy;
		var oldSynth;

		// we use the one-to-one equivalence of synths and tokens
		this.prevTokens.do { |token, i|
			var newSynth, args;
			if(names.isNil or: { names.includes(token) }) {
				args = argList.at(i);
				oldSynth = oldSynthList.at(i);

				newSynth = this.newSynth(token, nil, args, oldSynth);
				if(verbosity > 1) { ("replaced synth" + token).postln };
				"releasing old synth with id: %".format(oldSynthList.at(i)).postln;
				oldSynth.release;

				synthList.put(i, newSynth);
			}
		}
	}

	startGroup {
		group = group ?? {Group(server)};
	}

	startMonitor { |restart = false|

		if(monitor.isPlaying) {
			if(restart) { monitor.release } { ^this }
		};
		monitor = Synth(this.prefix(\monitor),
			[\out, bus ? 0, \in, busIndices.first, \amp, 0.1],
			target: group,
			addAction:\addAfter
		).register(true);

	}

	initBusses {
		var n;
		if(busIndices.isNil) {
			// allocate busses for maxBracketDepth plus tailBus (used in filter fades)
			n = numChannels * maxBracketDepth;
			busIndices = server.audioBusAllocator.alloc(n);
			tailBus = server.audioBusAllocator.alloc(numChannels);
			if(busIndices.isNil or: { tailBus.isNil }) {
				"not enough busses available! Please reboot the server"
				"or increase the number of audio bus channels in ServerOptions".throw
			};
			busIndices = busIndices + (0, numChannels .. (n-1));
		}
	}
	//////////////////// getting information about the resulting synth graph ////////////

	dumpStructure { |postDryIn = false|
		var header = String.fill(maxBracketDepth + 4, $-);
		var findBus = { |bus| busIndices.indexOf(bus) };
		header = [header, "  %  ", header, "\n"].join;
		argList.do { |args, i|
			var in, out, dryIn, token, arity;
			token = this.removePrefix(synthList.at(i).defName);
			header.postf(token);
			args = args.keep(-10); // keep the last 5 pairs which are the ones that were added by SynthStack

			if(args.isEmpty.not) {
				in = findBus.(args[1]);
				out = findBus.(args[3]);
				dryIn = findBus.(args[5]);

				in.do { "   ".post }; in.post;
				arity = operators[token];
				if(arity.notNil) {
					(arity - 2).do { "___".post };
					"__".post;
					(in + arity - 1).post;
				};
				"\n".post;
				if(postDryIn) { dryIn.do { "   ".post }; "(%)\n".postf(dryIn) };
				out.do { "   ".post }; out.postln;

			}
		}
	}


	///////////////// wrappers for UGen functions ///////////////////////

	// external interface

	filter { |name, func, multiChannelExpand, update = true, numChannels|
		this.addSynthDef(name, {
			var stenoSignal, signalNumChannels;
			signalNumChannels = min(numChannels ? this.numChannels, this.numChannels);
			stenoSignal = StenoSignal(signalNumChannels);
			stenoSignal.filter(func, multiChannelExpand ? expand, signalNumChannels);
			stenoSignal.writeToBus;
			if(verbosity > 0) { ("new filter: \"%\" with % channels\n").postf(name, signalNumChannels) };
		}, update);
	}

	quelle { |name, func, multiChannelExpand, update = true, numChannels|

		this.addSynthDef(name, {
			var stenoSignal, signalNumChannels;
			signalNumChannels = min(numChannels ? this.numChannels, this.numChannels);
			stenoSignal = StenoSignal(signalNumChannels);
			stenoSignal.quelle(func, multiChannelExpand ? expand, signalNumChannels);
			stenoSignal.writeToBus;
			if(verbosity > 0) { ("new quelle: \"%\" with % channels\n").postf(name, signalNumChannels) };
		}, update);
	}

	// TODO: shapes etc.
	struktur { |name, func, multiChannelExpand, update = true, numChannels|

		this.addSynthDef(name, {
			var stenoSignal, signalNumChannels;
			signalNumChannels = min(numChannels ? this.numChannels, this.numChannels);
			stenoSignal = StenoSignal(signalNumChannels, multiChannelExpand);
			func.value(stenoSignal.input, stenoSignal); // pass the signal object here, so func can use it
			stenoSignal.writeToBus;
			if(verbosity > 0) { ("new struktur: \"%\" with % channels\n").postf(name, signalNumChannels) };
		}, update);
	}

	operator { |name, func, arity = 2, multiChannelExpand, update = true|

		var updateSubgraph = this.prevTokens.includes(name) and: { operators[name] != arity };

		this.addSynthDef(name, {
			var numChannels = this.numChannels;
			var totalNumChannels = numChannels * arity;

			var stenoSignal = StenoSignal(totalNumChannels);
			var inputs = { |i|
				stenoSignal.filterInput(numChannels, i * numChannels);
			} ! arity;
			var outputs = func.value(*inputs.keep(arity).add(stenoSignal.controls));

			if(outputs.notNil) {
				outputs = outputs.asArray.keep(numChannels);
				stenoSignal.filterOutput(outputs, numChannels);
				stenoSignal.writeToBus;
			};

			if(verbosity > 0) { ("new operator: \"%\" with % channels and arity %\n").postf(name, numChannels, arity) };
		}, update, updateSubgraph);

		operators[name.asSymbol] = arity;
	}

	setter { |name ... keyValuePairs|
		forBy(1, keyValuePairs.lastIndex, 2, { |i|
			keyValuePairs[i] = keyValuePairs[i].reference
		});
		this.set(name, *keyValuePairs);
		// dummy synth
		this.addSynthDef(name, { FreeSelf.kr(\gate.kr(1) < 1) }, true, false);
	}


	declareVariables { |names|
		names.do { |name|
			name = name.asSymbol;
			if(variables[name].isNil) {
				"new variable as ".post;

				this.filter(name, { |input, controls|
					var bus, stored;


					// Bus to store the signal
					//    bus declaration inside synth func restores busses with
					//    correct channel numbers, e.g. when number of channels
					//    changed on the fly
					bus = Bus.audio(server, numChannels);

					// safe bus for introspection
					variables[name] = bus;

					stored = LinXFade2.ar(
						// stored signal
						//     (no feedback: != 0 only after first appearance of variable)
						inA: In.ar(bus, numChannels),

						// stored signal from previous cycle (limited)
						inB: Limiter.ar(InFeedback.ar(bus, numChannels), 8, 0.01),
						// only do InFeedback for first appearance of variable
						pan: (controls.feedback.abs * (controls.index < 1) * 2 - 1)
					)
					// multiply by feedback sign (0.sign == 0 >> 1)
					* Select.kr(controls.feedback.abs > 0, [1, controls.feedback.sign]);

					// \assignment can be increased for feeding in more than one signal
					Out.ar(bus, input * (controls.index < \assignment.kr(1)));

					Mix([
						// synth always feeds through input signal,
						// regardless of mix parameter (set externally)
						input,
						// the stored signal
						stored * controls[\env]
					])
				});
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
		var routingFunction, beginSerialFunction, dummyFunction, dummyOpeningFunction;
		// we always go through a limiter here.

		// LFSaw.de -- quite heavy processing and sound shaping, I'll rather reduce the signal (since there's likely lots of summation happening) and have the intended level up.
		// this.addSynthDef(\monitor, { |out, in, amp = 0.1|
		// 	Out.ar(out,
		// 		Limiter.ar(
		// 			In.ar(in, numChannels),
		// 			amp,
		// 			0.1
		// 		)
		// 	)
		// }, force:true);


		this.addSynthDef(\monitor, { |out, in, amp = 0.1, level = 0.9|
			Out.ar(out,
				Limiter.ar(
					LeakDC.ar(In.ar(in, numChannels)) * amp,
					level,
					0.05
				)
			)
		}, force:true);


		// nothing to do, just clean up bus, to be sure.
		// dummyOpeningFunction = {
		// 	var stenoSignal;

		// 	stenoSignal = StenoSignal(numChannels);
		// 	stenoSignal.filterInput;
		// 	ReplaceOut.ar(stenoSignal.outBus, Silent.ar(numChannels)); // umbrella
		// 	FreeSelfWhenDone.kr(stenoSignal.env);
		// };
		/*
		dummyOpeningFunction = {
		var stenoSignal;
		stenoSignal = StenoSignal(numChannels);
		FreeSelfWhenDone.kr(stenoSignal.env); // free synth if gate 0

		};
		*/


		// same for all closing brackets
		routingFunction = {
			var stenoSignal = StenoSignal(numChannels);
			stenoSignal.filterInput;
			stenoSignal.closeBracket;
			stenoSignal.writeToBus;
		};

		dummyFunction = {
			FreeSelf.kr(\gate.kr(1) < 1)
		};

		beginSerialFunction = {
			var stenoSignal = StenoSignal(numChannels);
			stenoSignal.beginSerial;
			// for now, the beginSerial method writes to bus.
			// later make a class method like StenoSignal.beginSerial, and StenoSignal.closeBracket
		};

		//this.addSynthDef('(', routingFunction, force:true);


		this.addSynthDef('(', beginSerialFunction, force:true);



		this.addSynthDef('[', dummyFunction, force:true);
		this.addSynthDef('{', dummyFunction, force:true);

		this.addSynthDef(')', routingFunction, force:true);
		this.addSynthDef(']', routingFunction, force:true);
		this.addSynthDef('}', routingFunction, force:true);


		this.addSynthDef('!', dummyFunction, force:true);
		this.addSynthDef('?', dummyFunction, force:true); // if not found use "?".

	}


	addSynthDef { |name, func, update = true, updateSubgraph = false, force = false|
		var def;
		if(variables.at(name).notNil) { Error("The token '%' is declared as a variable already.".format(name)).throw };
		if("()[]{}?!".find(name.asString).notNil  and: { force.not }) {
			Error("The token '%' cannot be overridden.".format(name)).throw
		};
		encyclopedia = encyclopedia ? ();
		encyclopedia.put(name, func);
		def = SynthDef(this.prefix(name), func).add;
		settings.addSynthDef(name, def);
		if(update) {
			fork {
				server.sync;
				this.sched {
					if(updateSubgraph) {
						this.resendSynths // for now, just update all
					} {
						this.resendSynths([name])
					}
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
				var args = this.calcNextArguments(token);
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
				var synth, args, currentSynth;
				if(i >= synthList.size) {
					"swapFunc: some inconsistency happened, nothing to see here, keep going ...".warn;
				} {
					args = this.calcNextArguments(token);
					currentSynth = synthList.at(i);
					synth = this.newSynth(token, i, args, currentSynth.nodeID); // place new synth after old
					currentSynth.release;
					synthList.put(i, synth);
					argList.put(i, args);
				};
			},
			keepFunc: { |token, i|
				var args, currentSynth, synth;
				if(i >= synthList.size) {
					"keepFunc: some inconsistency happened, nothing to see here, keep going ...".warn;
				} {
					args = this.calcNextArguments(token);
					argList.put(i, args);
					currentSynth = synthList.at(i);

					if(ignoreReplaceAll.not and: { argumentStack.replaceAll }) {
						synth = this.newSynth(token, i, args, currentSynth.nodeID); // place new synth after old
						currentSynth.release;
						synthList.put(i, synth);
					} {
						currentSynth.set(*args);
					}
				};
			},
			beginFunc: {
				if(server.serverRunning.not) { Error("server not running").throw };
				this.initArguments;
				this.startGroup;
				// add a limiter to the end of the signal chain
				this.startMonitor;
				this.beginFunction.value;
			},
			returnFunc: {
				if(verbosity > 1) { this.dumpStructure };
				argumentStack = nil;
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
		}
	}

	///////////////////////////////////
	// create new synth from token
	///////////////////////////////////

	newSynth { |token, i, args, target|
		var addAction;

		// LFSaw.de: if target not explicitely given (needed for replacement, to place new synth _after_ old):
		// if first in list, add synth to encapsulating group
		// otherwise add it after previous synth in list

		target = target ?? { synthList[i - 1] };
		addAction = if(target.isNil) {
			target = group;
			\addToHead
		} {
			\addAfter
		};


		token = token.asSymbol;
		if(encyclopedia.at(token).isNil) { token = '?' }; // silent

		^Synth(
			this.prefix(token),
			args,
			target: target,
			addAction: addAction
		)
	}

	////////////////////////////////////////////////////////
	// function used to step through the syntactic structure
	////////////////////////////////////////////////////////

	initArguments {
		argumentStack = StenoStack(busIndices);
		settings.startGraph;
	}

	calcNextArguments { |token|
		var args, arity, controls;

		token = token.asSymbol;
		argumentStack ?? { this.initArguments };

		args = switch(token,
			'(', { settings.push; argumentStack.beginSerial; },
			')', { settings.pop; argumentStack.endSerial; },
			'[', { settings.push; argumentStack.beginParallel;  },
			']', { settings.pop; argumentStack.endParallel;},
			'{', { settings.push; argumentStack.beginStack; },
			'}', { settings.pop; argumentStack.endStack; },
			'!', { argumentStack.beginReplaceAll; },
			// default case
			{
				controls = argumentStack.controls;
				arity = operators[token];

				// escape operators that occur outside a stack context
				if(arity.notNil and: { argumentStack.inOperatorStack.not }) {
					"Operator '%' used outside a stack. Better we ignore it.".format(token).warn;
					token = '?';
					argumentStack.pushLetter(token)
				} {
					if(arity.notNil) {
						argumentStack.pushOperator(token, arity)
					} {
						argumentStack.pushLetter(token)
					}
				}
			}
		);
		//"after %,  the argument index is %\n".postf(token, argumentStack.argumentIndex);
		//"% args: %\n".postf(token, args);

		args = settings.calcNextArguments(token, controls) ++ args  ++ [\tailBus, tailBus]; // append the necessary args, so they can't be overridden
		//"% args: %\n".postf(token, args);
		^args

	}


	*defaultPreProcessor {
		^#{ |str, steno|

			var newStr = str.class.new, currentClump = str.class.new, hasGap = false;

			if(str.isNil) {
				str = steno.cmdLine ? str.class.new; // do resend?
			} {
				str = str.replace("\n", " ");
			};

			// strip trailing whitespace
			if(str.notEmpty) {
				while { str[0].isSpace } { str = str.drop(1) };
				while { str[str.size - 1].isSpace } { str = str.drop(-1) };
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
					outerScope[\currentClump] = currentClump ++ char;
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
						if(char == $}) {
							currentClump = currentClump.replace(" ", "") ++ char;
						} {
							currentClump = currentClump ++ char;
						}
					}
				};
			}, false, false); // todo: check if we can avoid double checking below
			newStr = newStr ++ currentClump; // add rest.
			newStr = newStr.replace(" ", "");

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

	stenoDotStructure { |title = "untitled", attributes = "", variableNames, operators, labelAttributes = ""|
		var labelString = "", graphString = "", variableLinks = ();
		operators = operators.copy;
		this.do { |char, i|
			labelString = labelString ++ format("% [label=\"%\"; %];\n", i, char, labelAttributes);
		};
		labelString = labelString ++ format("% [label=\"%\"; %];\n", this.size, "out", labelAttributes);
		this.doBrackets({ |token, i, scope, outerScope|
			var arity = operators[token.asSymbol];
			if("([{".includes(token)) {
				if(outerScope.notNil) { // on opening a bracket: connect to previous
					scope[\prevNode] = outerScope[\prevNode];
				};
			};
			if("]".includes(token).not and: { arity.isNil }) { // no need to link forward to next bracket
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
			if(arity.notNil) {
				// print uplinks in operator stack graph
				scope[\upLinks].keep(arity.neg).do { |x|
					graphString = graphString ++ "% -> %;\n".format(x, i);
				};
				scope[\upLinks] = scope[\upLinks].drop(arity.neg);
				scope[\prevNode] = i;
				scope[\upLinks] = scope[\upLinks].add(i);
			} {

				// if serial, link to previous
				// if parallel, link to top.

				switch(scope[\modus],
					\parallel, { scope[\upLinks] = scope[\upLinks].add(i) },
					\stack, { scope[\upLinks] = scope[\upLinks].add(i) },
					{ scope[\prevNode] = i }
				);
			};

			// switch modus
			if(token == $() { scope[\modus] = \serial };
			if(token == $[) { scope[\modus] = \parallel };
			if(token == ${) { scope[\modus] = \stack };

			scope // return new current scope for next iteration

		}, false, false);

		if(variableNames.isCollection) {
			this.do { |char, i|
				var name = char.asSymbol;
				if(variableNames.includes(name)) {
					if(variableLinks.at(name).isNil) {
						variableLinks.put(name, i) // define a new variable
					} {
						"found:".postln;
						graphString = graphString ++ "% -> %;\n".format(variableLinks.at(name), i);
					}
				}
			};
		};
		graphString = graphString ++ "% -> %;\n".format(this.size - 1, this.size);

		// ^"digraph %\n{\n%\n%\n}\n}".format(title, attributes, labelString, graphString)
		^"\n\ndigraph %\n{\n%\n".format("", attributes) ++ labelString ++ "\n" ++ graphString ++ "}\n}"
	}



}

