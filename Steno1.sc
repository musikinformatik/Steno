Steno1 : Steno {
	initDiff {
		var oldSynthList;
		diff = VarDiffString.new(
			insertFunc: { |token, i, j|
				var args = this.calcNextArguments(token);
				var synth = this.newSynth(token, j, args);
				synthList = synthList.add(synth);
				argList = argList.add(args);
			},
			removeFunc: { |token, i, j|
				if(i >= oldSynthList.size) {
					"removeFunc: some inconsistency appeared, nothing to see here, keep going ...".warn;
				} {
					oldSynthList[i].release;
				};
			},
			keepFunc: { |token, i, j|
				var args, currentSynth, synth, target;
				if(j != synthList.size) {
					"keepFunc: some inconsistency happened, nothing to see here, keep going ...".warn;
				} {
					args = this.calcNextArguments(token);
					if(argumentStack.replaceAll) {
						synth = this.newSynth(token, j, args, oldSynthList[i].nodeID); // place new synth after old
						oldSynthList[i].release;
					} {
						synth = oldSynthList[i];
						synth.set(*args);
						target = synthList[j-1];
						if(target.isNil) {
							synth.moveToHead(group);
						} {
							synth.moveAfter(target);
						};


					};
					synthList = synthList.add(synth);
					argList = argList.add(args);

				};
			},
			beginFunc: {
				if(server.serverRunning.not) { Error("server not running").throw };
				this.initArguments;
				this.startGroup;
				// add a limiter to the end of the signal chain
				this.startMonitor;
				oldSynthList = synthList;
				synthList = [];
				argList = [];
			},
			returnFunc: { |src, trg |
				if(verbosity > 1) { this.dumpStructure };
				if(verbosity < 0) { src.postln; trg.postln; };
				argumentStack = nil;
			}
		)
	}
}
/*
(
t = Steno1.new;
t.verbosity = -1;
t.quelle(\a, { Blip.ar(Rand(4, 16)) * 0.2 });
t.quelle(\b, { Saw.ar(Rand(400, 700)) * 0.2 });
t.filter(\f, { |input| CombL.ar(input, 0.2, Rand(0.01, 0.02), Rand(0.4, 2) ) });

t.value("aafbaaf"); ""
t.value("!aafbaaf"); ""
t.value("faaf"); ""
t.value("aaf"); ""
t.diff.diff1;
t.value("faa"); ""
t.value("aaf"); ""
t.value("faa"); ""
t.value("abf"); ""
t.diff.diff2;
t.value("faa"); ""
t.value("aaf"); ""
t.value("faa"); ""
t.value("faaf"); ""
t.diff.diff3;
t.value("faaf"); ""

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
