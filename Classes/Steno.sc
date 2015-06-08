/*
todo: import sets from the web
*/


Steno {

	var <numChannels, <expand, <maxBracketDepth, <server, <bus;
	var <>quant, <settings, <globalSettings;
	var <encyclopedia, <operators;
	var <monitor, <diff;
	var <busIndices, <synthList, <argList, <variables;
	var <>preProcessor, <>preProcess = true, <cmdLine, <rawCmdLine;
	var <>verbosity = 1; // 0, 1, 2.

	var argumentStack;

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
			Error("bus must be audio rate and have at least have % channels".format(numChannels)).throw
		};
		bus = argBus;
		this.rebuild;
	}

	rebuild {
		fork {
			this.initBusses;
			this.rebuildSynthDefs;
			server.sync;
			this.resendSynths;
			this.startMonitor(restart: true);
		}
	}

	rebuildGraph {
		fork {
			this.freeAll;
			this.initBusses;
			this.rebuildSynthDefs;
			server.sync;
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
					diff.value(string)
					//diff.parse(string.as(Array), synthList.collect { |x| this.removePrefix(x.defName) }) // inefficient, but safe
					// if we use this one, we should use events instead of synths. then alsoprevTokens needs to be changed.
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
			[\out, bus ? 0, \in, busIndices.first, \amp, 0.1],
			addAction:\addAfter
		).register;

	}

	initBusses {
		var n;
		if(busIndices.isNil) {
			n = numChannels * maxBracketDepth;
			busIndices = server.audioBusAllocator.alloc(n);
			if(busIndices.isNil) {
				"not enough busses available! Please reboot the server"
				"or increase the number of audio bus channels in ServerOptions".throw
			};

			busIndices = busIndices + (0, numChannels .. n);
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

	// this represents the structure of the actual implementation
	// not complete

	plotStructure { |title = "untitled"|
		var window, run = true;
		var in, out, prevNode, curNode;
		window = Window(title);
		window.background = Color.black;
		window.onClose = { run = false };
		window.drawFunc = {
			var prevNodes = Array.newClear(busIndices.size);
			synthList.do { |synth, i|
				var nodeSize = 20;
				var args = argList.at(i);
				var name = this.removePrefix(synth.defName);
				var mul = Point(
					window.bounds.width - (nodeSize * 2) / busIndices.size,
					window.bounds.height - (nodeSize * 2) / synthList.size
				);
				if(args.isEmpty.not and: { name != ')' }) {
					in = busIndices.indexOf(args[1]);
					//in = busIndices.indexOf(args[5]);
					out = busIndices.indexOf(args[3]);
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

	dotStructure { |title, attributes|
		attributes = attributes ?? { "rankdir=LR;" };
		^this.cmdLine.stenoDotStructure(title ? cmdLine, attributes, variables.keys, operators)
	}


	///////////////// wrappers for UGen functions ///////////////////////

	// external interface

	filter { |name, func, multiChannelExpand, update = true, numChannels|
		numChannels = min(numChannels ? this.numChannels, this.numChannels);
		this.addSynthDef(name, {
			var stenoSignal, signalNumChannels;
			signalNumChannels = min(numChannels ? this.numChannels, this.numChannels);
			stenoSignal = StenoSignal(numChannels);
			stenoSignal.filter(func, multiChannelExpand ? expand, numChannels);
			stenoSignal.writeToBus;
			if(verbosity > 0) { ("new filter: \"%\" with % channels\n").postf(name, numChannels) };
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

		numChannels = min(numChannels ? this.numChannels, this.numChannels);
		this.addSynthDef(name, {
			var stenoSignal, signalNumChannels;
			signalNumChannels = min(numChannels ? this.numChannels, this.numChannels);
			stenoSignal = StenoSignal(numChannels, multiChannelExpand);
			func.value(stenoSignal.input, stenoSignal);
			stenoSignal.writeToBus;
			if(verbosity > 0) { ("new struktur: \"%\" with % channels\n").postf(name, numChannels) };
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
		var routingFunction, dummyOpeningFunction;
		// we always go through a limiter here.
		this.addSynthDef(\monitor, { |out, in, amp = 0.1|
			Out.ar(out,
				Limiter.ar(
					In.ar(in, numChannels),
					amp,
					0.1
				)
			)
		}, force:true);

		// same for all closing brackets
		// mix controls balance between serial bus result and outside bus
		routingFunction = { |in, out, dryIn, mix = 1, through = 0| // mix = 1: don't add outside in twice
			var input = In.ar(in, numChannels); // in: result of serial synths
			var oldSignal = In.ar(out, numChannels); // the old signal on the bus, mixed in by through
			var inputOutside = In.ar(dryIn, numChannels);  // dryIn: bus outside parenthesis
			var signalOnMixBus = input + (through * oldSignal);
			var output = XFade2.ar(inputOutside, signalOnMixBus, mix * 2 - 1);
			ReplaceOut.ar(in, Silent.ar(numChannels)); // clean up: overwrite channel with zero.
			XOut.ar(out, EnvGate.new, output); // overwrite the out channel with the new mix
		};

		// nothing to do, just clean up bus, to be sure.
		dummyOpeningFunction = { |in, out|
			ReplaceOut.ar(out, Silent.ar(numChannels)); // umbrella
			FreeSelf.kr(\gate.kr(1) < 1); // dummy synth, can be released
		};

		// begin serial: dry = in
		/*
		this.addSynthDef('(', { |in, out, dryIn, mix = 0, through = 0|
			var input = In.ar(in, numChannels); // dryIn: bus outside parenthesis
			var oldSignal = In.ar(out, numChannels);
			var output = XFade2.ar(input, through * oldSignal, mix * 2 - 1);
			ReplaceOut.ar(in, Silent.ar(numChannels)); // clean up: overwrite channel with zero.
			XOut.ar(out, EnvGate.new, output);
		}, force:true);
		*/


		this.addSynthDef('(', routingFunction, force:true);

		this.addSynthDef('[', dummyOpeningFunction, force:true);
		this.addSynthDef('{', dummyOpeningFunction, force:true);

		this.addSynthDef(')', routingFunction, force:true);
		this.addSynthDef(']', routingFunction, force:true);
		this.addSynthDef('}', routingFunction, force:true);

		this.addSynthDef('?', { FreeSelf.kr(\gate.kr(1) < 1); }, force:true); // if not found use this.

	}


	addSynthDef { |name, func, update = true, updateSubgraph = false, force = false|
		if(variables.at(name).notNil) { Error("The token '%' is declared as a variable already.".format(name)).throw };
		if("()[]{}?".find(name.asString).notNil  and: { force.not }) {
			Error("The token '%' cannot be overridden.".format(name)).throw
		};
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
		};
	}

	///////////////////////////////////
	// create new synth from token
	///////////////////////////////////

	newSynth { |token, i, args|
		var target = synthList[i - 1];
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
		argumentStack = StenoStack(busIndices);
	}

	calcNextArguments { |token, i|
		var args, thisSetting, arity, controls;

		token = token.asSymbol;
		argumentStack ?? { this.initArguments };
		controls = argumentStack.controls;

		args = switch(token,
			'(', { argumentStack.beginSerial },
			')', { argumentStack.endSerial },
			'[', { argumentStack.beginParallel },
			']', { argumentStack.endParallel },
			'{', { argumentStack.beginStack },
			'}', { argumentStack.endStack },
			// default case
			{
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


		thisSetting = globalSettings.copy ? ();
		settings.at(token) !? { thisSetting.putAll(settings.at(token)) };

		// we allow functions in settings to expand dependent on current state
		thisSetting.keysValuesDo { |key, val|
			args = args.add(key).add(val.value(controls))
		};

		//"% args: %\n".postf(token, args);
		^args

	}


	*defaultPreProcessor {
		^#{ |str, steno|
			var newStr = str.class.new, doResend = false, currentClump = str.class.new, hasGap = false, char;

			if(str.isNil) {
				str = steno.cmdLine ? str.class.new; doResend = true;
			} {
				if(str[0] == $!) { doResend = true; str = str.drop(1); };
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

	stenoDotStructure { |title = "untitled", attributes = "", variableNames, operators|
		var labelString = "", graphString = "", variableLinks = ();
		operators = operators.copy;
		this.do { |char, i|
			labelString = labelString ++ format("% [label=\"%\"];\n", i, char);
		};
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
			if("])".includes(token)) {
				// print uplinks in parallel graph
				scope[\upLinks].do { |x|
					graphString = graphString ++ "% -> %;\n".format(x, i);
				};
				// now we can move outside:
				scope = outerScope;
			};
			if(arity.notNil) {
				// print uplinks in parallel graph
				format("index: %\n%\n\n", i, scope.cs).postln;
				scope[\upLinks].keep(arity.neg).do { |x|
					graphString = graphString ++ "% -> %;\n".format(x, i).postln;
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
						graphString = graphString ++ "% -> %;\n".format(variableLinks.at(name), i);// ERRROR HERE?
					}
				}
			};
		};
		// ^"digraph %\n{\n%\n%\n}\n}".format(title, attributes, labelString, graphString)
		^"digraph %\n{\n%\n".format("", attributes) ++ labelString ++ "\n" ++ graphString ++ "}\n}"
	}



}

