
Steno {

	var <numChannels, <expand, <maxBracketDepth, <server, <bus;
	var <>quant, <settings;
	var <encyclopedia, <operators;
	var <monitor, <diff;
	var <busIndices, <synthList, <argList, <variables;
	var <fadeBus;
	var <>preProcessor, <>preProcess = true, <cmdLine, <rawCmdLine;
	var <>verbosity = 1; // 0, 1, 2.
	var <group;

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
		group = nil;
	}

	set { |name ... keyValuePairs|
		this.sched {
			settings.set(name, keyValuePairs);
			this.eval(cmdLine);
		}
	}

	setGlobal { |... keyValuePairs|
		this.sched {
			settings.setGlobal(keyValuePairs);
			this.eval(cmdLine);
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
			if(string[0] == $!) { this.freeAll; string = cmdLine = string.drop(1) };
			if(string.last == $!) { this.freeAll; string = cmdLine = string.drop(-1) };
			if(verbosity > 0) { string.postcs };
			diff.value(string)
			//diff.parse(string.as(Array), synthList.collect { |x| this.removePrefix(x.defName) }) // inefficient, but safe
			// if we use this one, we should use events instead of synths. then alsoprevTokens needs to be changed.
		} {
			server.closeBundle(server.latency);
		}

	}

	prevTokens {
		^diff.prevTokens.collect { |x| x.asSymbol }
	}

	resendSynths { |names| // names are symbols
		// we use the one-to-one equivalence of synths and tokens
		this.prevTokens.do { |token, i|
			var newSynth, args;
			if(names.isNil or: { names.includes(token) }) {
				args = argList.at(i);
				newSynth = this.newSynth(token, i, args);
				if(verbosity > 1) { ("replaced synth" + token).postln };
				"releasing old synth with id: %".format(synthList.at(i)).postln;
				synthList.at(i).release;
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
		).register;

	}

	initBusses {
		var n;
		if(busIndices.isNil) {
			// allocate busses for maxBracketDepth plus fadeBus (used in filter fades)
			n = numChannels * maxBracketDepth;
			busIndices = server.audioBusAllocator.alloc(n);
			fadeBus = server.audioBusAllocator.alloc(numChannels);
			if(busIndices.isNil || {fadeBus.isNil}) {
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
			args = args.keep(-8); // keep the last 4 pairs which are the ones that were added by SynthStack

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

	dotStructure { |title, attributes, labelAttributes|
		attributes = attributes ?? { "rankdir=LR;\nfontname=Courier;\nlabel=\"\n\n%\"".format(cmdLine) };
		labelAttributes = labelAttributes ?? { "fontname=Courier" };
		^this.cmdLine.stenoDotStructure(title ? cmdLine, attributes, variables.keys, operators, labelAttributes)
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
					var feedback = \feedback.kr(0);
					var bus = Bus.audio(server, numChannels);
					var in = XFade2.ar(
						inA: In.ar(bus, numChannels),
						inB: InFeedback.ar(bus, numChannels) * feedback.sign,
						pan: feedback.abs.linlin(0, 1, -1, 1)
					);
					variables[name].free; variables[name] = bus;

					// \assignment can be increased for feeding in more than one signal
					Out.ar(bus, input * (\tokenIndex.kr < \assignment.kr(1)));

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
					In.ar(in, numChannels) * amp,
					level,
					0.05
				)
			)
		}, force:true);

		// same for all closing brackets
		// mix controls balance between serial bus result and outside bus
		routingFunction = {// |in, out, dryIn, mix = 1, through = 0| // mix = 1: don't add outside in twice
			var oldSignal, inputOutside, stenoSignal, signal, signalOnMixBus;

			stenoSignal = StenoSignal(numChannels);
			stenoSignal.filterInput;
			oldSignal = In.ar(stenoSignal.outBus, numChannels); // the old signal on the bus, mixed in by through
			inputOutside = In.ar(stenoSignal.dryIn, numChannels);  // dryIn: bus outside parenthesis

			signal = XFade2.ar(inputOutside, stenoSignal.input, MulAdd(stenoSignal.mix, 2, -1));

			signal = Mix.ar([
				signal,
				oldSignal * max(
					stenoSignal.through.varlag(stenoSignal.fadeTime, start: 0),
					1 - stenoSignal.env
				)
			]);   // fade old input according to gate, signal is supposed to fade out itself.

			FreeSelfWhenDone.kr(stenoSignal.env); // free synth if gate 0
			ReplaceOut.ar(stenoSignal.inBus, Silent.ar(numChannels)); // clean up: overwrite channel with zero.

			stenoSignal.addOutput(signal);
			stenoSignal.writeToBus;
		};

		// nothing to do, just clean up bus, to be sure.
		// dummyOpeningFunction = {
		// 	var stenoSignal;

		// 	stenoSignal = StenoSignal(numChannels);
		// 	stenoSignal.filterInput;
		// 	ReplaceOut.ar(stenoSignal.outBus, Silent.ar(numChannels)); // umbrella
		// 	FreeSelfWhenDone.kr(stenoSignal.env);
		// };
		dummyOpeningFunction = {
			var stenoSignal;
			stenoSignal = StenoSignal(numChannels);
			stenoSignal.quelle(nil, true, numChannels);
			FreeSelfWhenDone.kr(stenoSignal.env); // free synth if gate 0

			stenoSignal.writeToBus;
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
		var def;
		if(variables.at(name).notNil) { Error("The token '%' is declared as a variable already.".format(name)).throw };
		if("()[]{}?".find(name.asString).notNil  and: { force.not }) {
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
					synth = this.newSynth(token, i, args ++ [\replacement, 1], currentSynth.nodeID); // place new synth after old
					currentSynth.release;
					synthList.put(i, synth);
					argList.put(i, args);
				};
			},
			keepFunc: { |token, i|
				var args;
				if(i >= synthList.size) {
					"keepFunc: some inconsistency happened, nothing to see here, keep going ...".warn;
				} {
					args = this.calcNextArguments(token);
					synthList.at(i).set(*args);
					argList.put(i, args);
				};
			},
			beginFunc: {
				if(server.serverRunning.not) { Error("server not running").throw };
				this.initArguments;
				this.startGroup;
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
		target = target ?? {synthList[i - 1]};
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

		args = settings.calcNextArguments(token, controls) ++ args  ++ [\fadeBus, fadeBus]; // append the necessary args, so they can't be overridden
		//"% args: %\n".postf(token, args);
		^args

	}


	*defaultPreProcessor {
		^#{ |str, steno|

			var newStr = str.class.new, doResend = false, currentClump = str.class.new, hasGap = false;

			if(str.isNil) {
				str = steno.cmdLine ? str.class.new; doResend = true;
			} {
				if(str.first == $!) { doResend = true; str = str.drop(1); };
				if(str.last == $!) { doResend = true; str = str.drop(-1); };
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

