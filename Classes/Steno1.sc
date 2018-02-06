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
s.waitForBoot {
t = Steno1.new;
t.verbosity = -1;
t.quelle(\a, { SinOsc.ar(Rand(200, 2130)) }); // quelle (aka source) produces sound
t.filter(\f, { |input| LFPulse.kr(ExpRand(1, 10), 0, Rand(0.1, 0.5)) * input }); // filter processes sound
t.filter(\g, { |input| CombL.ar(input, 0.2, Rand(0.03, 0.2), 1.3) });
}
)

t.value("diff0/aafbaafb"); ""
t.value("aafb"); ""
t.value("aafbaafb"); ""
t.value("aafb"); ""
t.value("diff1/aafbaafb"); ""
t.value("diff0/aafb"); ""

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

// diff0 and diff1 replace synths whose position shifts
// relative to retained synths
t.diff.diff0;
t.value("faa"); ""
t.value("aaf"); ""
t.value("faa"); ""
t.value("aaf"); ""

// diff2 and diff3 retain synths whenever possible

t.diff.diff2;
t.value("faa"); ""
t.value("aaf"); ""
t.value("faa"); ""
t.value("aaf"); ""

// diff0 retains towards the end
t.diff.diff0;
t.value("faafff"); ""
t.value("fafaff"); ""
t.value("faafff"); ""
t.value("fafaff"); ""

// diff1 retains towards the front
t.diff.diff1;
t.value("faafff"); ""
t.value("fafaff"); ""
t.value("faafff"); ""
t.value("fafaff"); ""

// diff2 retains towards the end
t.diff.diff2;
t.value("faafff"); ""
t.value("fafaff"); ""
t.value("faafff"); ""
t.value("fafaff"); ""

// diff3 retains towards the front
t.diff.diff3;
t.value("fafffa"); ""
t.value("faffaf"); ""
t.value("fafaff"); ""
t.value("faafff"); ""
t.value("aaffff"); ""
*/
