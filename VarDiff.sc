/*

Alternative diff for Steno

VarDiff::install(aSteno) puts it to use

*/

VarDiff {

	var <>prevTokens;
	var <>diffFunc;
	var <>steno;
	var <>beginFunc, <>returnFunc;

	*new {
		^super.new.init
	}

	install { | argSteno |
		steno = argSteno;
		// hack to get beging and return funcs w/o modifying Steno
		beginFunc = steno.diff.beginFunc;
		returnFunc = steno.diff.returnFunc;
		steno.diff = this;
	}

	init {
		prevTokens = [];
		this.diff0;
	}

	display { |tokens, deletions, newSynthList|
		var display = String.fill(prevTokens.size, $_);
		deletions.do { | i | display[i] = prevTokens[i] };
		display.postln;
		display = tokens.collect { | token, i |
			var synthIndex = newSynthList[i].first;
			if (synthIndex == \newSynth) { token } { token.toUpper }
		};
		display.postln;
	}

	value { | tokens |
		var curSynthList, deletions, newSynthList;
		tokens = tokens.as(Array);

		#deletions, newSynthList = diffFunc.value(prevTokens, tokens);

		// deletions: array of indices
		// newSynthList: array of [ sourceindex|\newSynth, token ]
		// 			and tokens[i] == newSynthList[i][1]

		this.display(tokens, deletions, newSynthList);

		curSynthList = steno.synthList;
		steno.synthList = Array(newSynthList.size);
		steno.argList = Array(newSynthList.size);
		beginFunc.value;
		steno.server.makeBundle(steno.server.latency, {

			deletions.do { | i | curSynthList[i].release };
			newSynthList.postln;

			newSynthList.do { | pair, i |
				var token, synthIndex;
				var synth, args, target;
				#synthIndex, token = pair;
				[i, steno.synthList].postln;
				args = steno.calcNextArguments(token);

				if (synthIndex == \newSynth) {
					synth = steno.newSynth(token, i, args);
				} {
					if(steno.argumentStack.replaceAll) {
						synth = steno.newSynth(token, i, args, curSynthList[synthIndex].nodeID); // place new synth after old
						curSynthList[synthIndex].release;
					} {
						synth = curSynthList[synthIndex];
						synth.set(*args);
					};

					target = steno.synthList[i-1];
					if(target.notNil) {
						synth.moveAfter(steno.synthList[i -1]);
					} {
						synth.moveToHead(steno.group);
					};
				};

				steno.synthList.add(synth);
				steno.argList.add(args);
			};
		});

		returnFunc.value;
		prevTokens = tokens;
	}

	// levenshtein distance,
	// takes source and target collections,
	// returns difference matrix as source.size target.sized arrays

	*ld { |source, target|
		var m, n, d, cost;
		m = source.size;
		n = target.size;
		d = m.collect {| i | 0.dup(n).addFirst(i + 1) };
		d = d.addFirst( (0..n) );
		n.do { | j |
			j = j + 1;
			m.do { | i |
				i = i + 1;
				cost = if(source[i-1] == target[j-1]) { 0 } { 1 };
				d[i][j] = (d[i-1][j] + 1) 				// deletion
				min: (d[i][j-1] + 1) 					// insertion
				min: (d[i-1][j-1] + cost);				// substitution
			}
		};
		^d
	}

	*parseld { | table, source, target |
		var choices;  // 0 retain, 1 insert, 2 delete
		var spos = table.size -1;
		var tpos = table[0].size - 1;
		var deletions;					// array of indices of elements in s not used in t
		var tokenPairs;						// array of pairs corresponding to each token in t
		// [nil, token] -> new token
		// [i, token] -> s[i] is used in t
		deletions = Array(source.size + 1);
		tokenPairs = Array(target.size + 1);
		while {
			(spos > 0) and: { tpos > 0 }
		} {
			// hack in a preference for contiguous characters
			// so txtest -> test takes txTEST rather than TxtEST
			if (source[spos-1] == target[tpos-1]) {
				spos = spos - 1;
				tpos = tpos - 1;
				tokenPairs.add([spos, source[spos]]);
			} {
				if (table[spos - 1][tpos] <= table[spos][tpos-1]) {
					spos = spos - 1;
					deletions.add(spos)
				} {
					tpos = tpos -1;
					tokenPairs.add([\newSynth, target[tpos]])
				};
			};
		};

		// now get the leftovers, which will be either in the source or the target
		spos.reverseDo { | i | deletions = deletions.add(i) };
		tpos.reverseDo {|i| tokenPairs = tokenPairs.add([\newSynth, target[i]]) };

		^[deletions.reverse, tokenPairs.reverse]
	}

	*study { |s, t|
		var sa, ta, d;
		d = VarDiff.ld(s, t);
		d = d.collect({| l, i | l.addFirst((" " ++ s)[i]) });
		d = d.addFirst("  " ++ t).as(Array);
		^d
	}

	diff0 {
		diffFunc = {| prevTokens, tokens |
			VarDiff.parseld(
				VarDiff.ld(prevTokens, tokens),
				prevTokens,
				tokens
			)
		}
	}

	diff1 {
		diffFunc = {| prevTokens, tokens |
			var d, n, s;
			s = prevTokens.size - 1;
			prevTokens= prevTokens.reverse;
			tokens = tokens.reverse;
			#d, n = VarDiff.parseld(VarDiff.ld(prevTokens, tokens), prevTokens, tokens);
			d = s - d;
			n = n.collect { | pr | [s - pr[0], pr[1] ] };
			n = n.reverse;
			[d,n]
		}
	}

	diff2 {
		diffFunc = {| prevTokens, tokens |
			var d, n, prevPos = ();
			prevTokens.do { | c, i |
				prevPos[c] = prevPos[c].add(i)
			};
			n = tokens.collect { | c |
				[prevPos[c].pop ? \newSynth, c]
			};
			d = Array(prevTokens.size);
			prevPos.do(d.add(_));
			[d.flat,n]
		}
	}

	diff3 {
		diffFunc = {| prevTokens, tokens |
			var d, n, prevPos = ();
			prevTokens.do{ | c, i |
				prevPos[c] = (prevPos[c] ? []).addFirst(i)
			};
			n = tokens.collect { | c |
				[prevPos[c].pop ? \newSynth, c]
			};
			d = Array(prevTokens.size);
			prevPos.do(d.add(_) );
			[d.flat,n]
		}
	}
}
/*
VarDiff.parse(VarDiff.ld("rabcd", ""), "rabcd", "").do(_.postln)

40 ?? {20} ?? { 30}
{ 20}, {30})
*/

/*
(
t = Steno.new;
VarDiff.new.install(t);
t.diff.diff1;
t.quelle(\a, { Blip.ar(Rand(4, 16)) * 0.2 });
t.quelle(\b, { Saw.ar(Rand(400, 700)) * 0.2 });
t.filter(\f, { |input| CombL.ar(input, 0.2, Rand(0.01, 0.02), Rand(0.4, 2) ) });
t.diff.diff0;
t.value("!faa"); ""

t.value("aafbaaf"); ""
t.value("faaf"); ""
t.value("aaf"); ""
t.diff.diff1;
t.value("faa"); ""
t.value("aaf"); ""
t.value("faa"); ""
t.value("aaf"); ""
t.diff.diff2;
t.value("faa"); ""
t.value("aaf"); ""
t.value("faa"); ""
t.value("aaf"); ""
t.synthList
)
t.value("baba ffffbbb");
t.value("aaaafa");
t.value("(aaf)(aaf)bb");
t.value("!(aaf)(aaf)bb");
t.value("(!aaf)(aaf)bb");

t.value("")

s.makeWindow
s.scope
s.queryAllNodes

*/
