/*
Alternative to DiffString.
Different approaches to matching characters between
the source and target strings are implemented.
An approach is selected with methods uncharismatically named "diff0, diff1, diff2"


{ | char, sourceIndex, targetIndex|

for deleteFunc targetIndex is Nil
for insertFunc sourceIndex is Nil


*/

VarDiffString {
	var <>insertFunc, <>removeFunc, <>keepFunc;
	var <>beginFunc, <>returnFunc, <>testFunc;

	var prevTokens;
	var <>diffFunc;

	*new { |insertFunc, removeFunc, keepFunc, beginFunc, returnFunc, testFunc|
		^super.newCopyArgs(insertFunc, removeFunc, keepFunc, beginFunc, returnFunc, testFunc ? true).init
	}

	// Steno requires this to be a list of chars
	prevTokens {
		^prevTokens.as(Array)
	}

	init { prevTokens = ""; this.diff0; }
	diff { | tokens |
		var changes = diffFunc.value(prevTokens, tokens);
		prevTokens = tokens;
		^changes
	}
	value { | tokens |
		var curSynthList, changes, newArgList, display, diffSelector, tokenList;
		var token, si;
		var synth, args;
		var tokenArray =[], target;
		var sString, tString;

		if (tokens.includes($/)) {
			tokenList = tokens.postln.split($/);
			tokens = tokenList.last;
			diffSelector = tokenList.first;
			this.perform(diffSelector.asSymbol);
		};

		sString = prevTokens.copy;
		tString = tokens.copy;
		changes = diffFunc.value(prevTokens, tokens);
		beginFunc.value;
		changes.do { | vals |
			var token, sourceIndex, targetIndex;
			#token, sourceIndex, targetIndex = vals;
			if(targetIndex.isNil) {
				removeFunc.(token, sourceIndex, targetIndex);
			} {
				if (sourceIndex.isNil) {
					insertFunc.(token, sourceIndex, targetIndex);
				} {
					keepFunc.(token, sourceIndex, targetIndex);
					sString = sString.put(sourceIndex, token.toUpper);
					tString = tString.put(targetIndex, token.toUpper);

				};
			};

		};
		returnFunc.(sString, tString);
		prevTokens = tokens;
	}
	// levenshtein distance,
	// takes source and target collections,
	// returns difference matrix as source.size target.sized arrays

	*ld { | s,t|
		var m,n, d, cost;
		m = s.size;
		n = t.size;
		d = m.collect {| i |  0.dup(n).addFirst(i + 1) };
		d = d.addFirst( (0..n) );
		n.do { | j |
			j = j + 1;
			m.do { | i |
				i = i + 1;
				if (s[i-1] == t[j-1]) {
					cost = 0
				} {
					cost = 1;
				};
				d[i][j] = (d[i-1][j] + 1) 				// deletion
				min: (d[i][j-1] + 1) 					// insertion
				min: (d[i-1][j-1] + cost);				// substitution

			}
		};
		^d;
	}

	*parseld { | table, s, t |
		var changes;  // array of [token, sourceIndex | nil, targetIndex | nil]
		var sourceIndex = table.size - 1;		// sourceIndex
		var targetIndex = table[0].size - 1;	// targetIndex

		changes = Array(s.size  + t.size + 1);
		while{ (sourceIndex >0) && (targetIndex >0) } {
			// hack in a preference for contiguous characters
			// so txtest -> test takes txTEST rather than TxtEST
			if (s[sourceIndex-1] == t[targetIndex-1]) {
				sourceIndex = sourceIndex -1; targetIndex = targetIndex-1;
				changes.add([s[sourceIndex], sourceIndex, targetIndex]);
			} {
				if (table[sourceIndex -1][targetIndex] <= table[sourceIndex][targetIndex-1])
				{ sourceIndex = sourceIndex -1; changes.add([s[sourceIndex], sourceIndex, nil]);}
				{ targetIndex = targetIndex-1; changes.add([t[targetIndex], nil, targetIndex])};
			};
		};
		// now get the leftovers, which will be either in the source or the target
		sourceIndex.reverseDo{ | sourceIndex | changes.add([s[sourceIndex], sourceIndex, nil]);};
		targetIndex.reverseDo{| targetIndex | changes.add([t[targetIndex], nil, targetIndex])};
		^changes.reverse;
	}

	*study { |s, t| var sa, ta, d;
		d= VarDiffString.ld(s, t);
		d = d.collect({| l, i | l.addFirst((" " ++ s)[i]) });
		d = d.addFirst(("  " ++ t).collectAs({|x| x}, Array));
		^d
	}

	diff0 {
		diffFunc = {| prevTokens, tokens |
			VarDiffString.parseld(VarDiffString.ld(prevTokens, tokens), prevTokens, tokens);
		}
	}

	diff1 {
		diffFunc = {| prevTokens, tokens |
			var changes, psz, tsz, reversedChanges;
			psz = prevTokens.size - 1;
			tsz = tokens.size - 1;
			prevTokens = prevTokens.reverse;
			tokens = tokens.reverse;
			changes = VarDiffString.parseld(VarDiffString.ld(prevTokens, tokens), prevTokens, tokens);
			changes = changes.collect { | tst |	//token, source, target
				tst[1] !? { tst[1] = psz - tst[1]};
				tst[2] !? { tst[2] = tsz - tst[2]};
				tst;
			};
			changes.reverse;
		}
	}

	diff2 {
		diffFunc = {| prevTokens, tokens |
			var d, n, prevPos = ();
			prevTokens.do{ | c, i |
				prevPos[c] = prevPos[c].add(i)
			};
			tokens.do { | c, i |
				n = n.add([c, prevPos[c].pop, i])
			};
			prevPos.keysValuesDo{ |k, v | if (v.size >0) {d = d ++ [k,v, nil].flop } };
			d ++ n;
		}
	}

	diff3 {
		diffFunc = {| prevTokens, tokens |
			var sz, d, n, prevPos = ();
			prevTokens.do{ | c, i |
				prevPos[c] = prevPos[c].add(i)
			};
			sz = tokens.size - 1;
			tokens.reverseDo { | c, i |
				n = n.add([c, prevPos[c].pop, sz - i])
			};
			n = n.reverse;
			prevPos.keysValuesDo{ |k, v | if (v.size >0) { d = d ++ [k,v, nil].flop } };
		}
	}
}
/*
s.waitForBoot {
t = Steno1.new;

// define a few letters
t.quelle(\a, { SinOsc.ar(Rand(200, 2130)) }); // quelle (aka source) produces sound
t.filter(\f, { |input| LFPulse.kr(ExpRand(1, 10), 0, Rand(0.1, 0.5)) * input }); // filter processes sound
t.filter(\g, { |input| CombL.ar(input, 0.2, Rand(0.03, 0.2), 1.3) });

t.verbosity = -1;

};

// use them in code:
t.("af");
t.("afg");
t.("aafgafg");
t.("afg");
t.("aafgafg");

t.diff.diff1;
t.("afg");
t.("aafgafg");
t.("afg");
t.("aafgafg");

t.diff.diff2;
t.("aafgaffg");
t.("aafgafg");
t.("aafgaffg");

t.diff.diff3;
t.("aafgaffg");
t.("aafgafg");
t.("aafgaffg");

// selectively redefine the interpretation
t.quelle(\a, { Blip.ar(ExpRand(1, 130), Rand(3, 100)) });
t.filter(\g, { |input| RLPF.ar(CombL.ar(distort(input * 3), 0.1, Rand(0.01, 0.2), 0.3), LFDNoise1.kr(0.5).exprange(200, 10000), 0.2) });

// parentheses structure the signal flow
t.("[(af)(ag)(af)]f");
t.cmdLine
t.value; // resend everything
s.plotTree; // show nodes
(
t = Steno1.new;
t.quelle(\a, { Blip.ar(Rand(4, 16)) * 0.2 });
t.quelle(\b, { Saw.ar(Rand(400, 700)) * 0.2 });
t.filter(\f, { |input| CombL.ar(input, 0.2, Rand(0.01, 0.02), Rand(0.4, 2) ) });
t.value("!faa"); ""

t.value("aafbaaf"); ""
t.verbosity = -1;

t.value("faaf"); ""
t.value("aaf"); ""
t.value("aaff"); ""
t.value("aafff"); ""

t.diff.diff1;
t.value("aaff"); ""

t.value("faa"); ""
t.value("aaf"); ""
t.value("faa"); ""
t.value("aaf"); ""
t.diff.diff3;
t.value("faa"); ""
t.value("aaff"); ""
t.value("faaf"); ""
t.value("aaf"); ""
t.synthList
)
t.value("ffffbbb baba");
t.value("!aaaafa");
t.value("(aaf)(aaf)bb");
t.value("!(aaf)(aaf)bb");
t.value("(!aaf)(aaf)bb");

t.value("")

s.makeWindow
s.scope
s.queryAllNodes

*/

/*
z : Steno {
	initDiff {
		var oldSynthList;
		diff = DiffString(
			insertFunc: { |token, i, j|
				var args = this.calcNextArguments(token);
				var synth = this.newSynth(token, j, args);
				synthList[j] = synth;
				argList[j] = args;
			},
			removeFunc: { |token, i, j|
				if(i >= oldSynthList.size) {
					"removeFunc: some inconsistency appeared, nothing to see here, keep going ...".warn;
				} {
					oldSynthList[i].release;
				};
			},
			keepFunc: { |token, i, j|
				var args, currentSynth, synth;
				if(j >= synthList.size) {
					"keepFunc: some inconsistency happened, nothing to see here, keep going ...".warn;
				} {
					args = this.calcNextArguments(token);

					if(argumentStack.replaceAll) {
						synth = this.newSynth(token, j, args, currentSynth.nodeID); // place new synth after old
						oldSynthList[i].release;
					} {
						synth = oldSynthList[i];
						synth.set(*args);
					};
					synthList[j] = synth;
					argList[j] = args;
				};
			},
			beginFunc: {
				if(server.serverRunning.not) { Error("server not running").throw };
				this.initArguments;
				this.startGroup;
				// add a limiter to the end of the signal chain
				this.startMonitor;
				server.openBundle;
			},
			returnFunc: {
				server.closeBundle;
				if(verbosity > 1) { this.dumpStructure };
				argumentStack = nil;
			}
		)
	}
}


*/