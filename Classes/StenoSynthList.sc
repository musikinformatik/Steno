/*

this class serves as a client side array representation of a server side list of synths.
any changes in the StenoSynthList are mirrored in the nodes on the server side.

*/

StenoSynthList {

	var <server, <group;
	var <synths, <arguments;

	*new { |server, group, synths, arguments|
		^super.newCopyArgs(server, group).init
	}

	init {
		synths = [];
		arguments = [];
	}

	size {
		^synths.size
	}

	release {
		synths.do(_.release)
	}

	freeAll {
		this.release;
		this.init;
	}

	clear {
		this.release;
		this.init;
		group.free;
	}

	add { |name, args|
		var synth = this.newSynth(name, args, synths.last);
		synths = synths.add(synth);
		arguments = arguments.add(args);
	}

	insert { |index, name, args|
		var synth = this.newSynth(name, args, synths.at(index));
		synths = synths.insert(index, synth);
		arguments = arguments.insert(index, args);
	}

	put { |index, name, args|
		var currentSynth = synths.at(index);
		var synth = this.newSynth(name, args, currentSynth); // place new synth after old
		currentSynth.release;
		synths.put(index, synth);
		arguments.put(index, args);
	}

	removeAt { |index|
		synths.removeAt(index).release;
		arguments.removeAt(index);
	}

	set { |index, args|
		var synth = synths.at(index);
		if(synth.isNil) { "StenoSynthList: no synth at index %".format(index); ^this };
		synth.set(*args);
		arguments.put(index, args);
	}

	getName { |index|
		var synth = synths.at(index);
		^synth !? { synth.defName }
	}

	newSynth { |name, args, target|
		var addAction;

		addAction = if(target.isNil) {
			target = group;
			\addToHead
		} {
			\addAfter
		};

		^Synth(name, args, target: target, addAction: addAction)
	}

	resendSynths { |names| // names are symbols
		synths.do { |oldSynth, i|
			var newSynth, args;
			var name = oldSynth.defName;
			if(names.isNil or: { names.includes(name) }) {
				this.put(i, name, arguments.at(i))
			}
		}
	}

	startGroup {
		if(group.isPlaying.not) {
			this.group = Group(server).register
		}
	}

	group_ { |argGroup|
		group = argGroup;
		if(synths.notEmpty) {
			synths.first.moveToTail(group);
			synths.doAdjacentPairs { |a, b| b.moveAfter(a) };
		}
	}

}
