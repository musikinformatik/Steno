
StenoSettings {
	var <globalSettings, <synthSettings, <msgFuncs, <current;
	var <>lexicalScope = true;
	var stack;
	var internalArgs = #[\in, \out, \dryIn, \through, \gate, \tailBus];

	*new { ^super.new.init }

	init {
		globalSettings = ();
		synthSettings = ();
		msgFuncs = ();
		current = ();
	}

	set { |name, keyValuePairs|
		name = name.asSymbol;
		synthSettings[name] = (synthSettings[name] ? ()).putPairs(keyValuePairs);
	}

	get { |name, key|
		var setting = synthSettings[name];
		^setting !? { setting[key] }
	}

	setGlobal { |keyValuePairs|
		globalSettings.putPairs(keyValuePairs);
	}

	getGlobal { |key|
		^globalSettings[key]
	}

	addSynthDef { |name, synthDef|
		var a = "", b = "", count = 0;
		synthDef.desc.controls.do { |c|
			if(internalArgs.includes(c.name).not) {
				a = a ++ c.name ++ " = " ++ c.defaultValue ++ ", ";
				b = b ++ "'%', %.value(controls ? ()), ".format(c.name, c.name);
				count = count + 1;
			};
		};
		msgFuncs[name] = if(count > 0) {
			"{ |controls, %| \n\t[%]\n}".format(a.drop(-2), b.drop(-2)).interpret;
		} { #[] };
	}

	push {
		stack = stack.add(current);
		current = if(lexicalScope) { current.copy } { () };
	}

	pop {
		current = stack.pop
	}

	startGraph {
		stack = nil;
		current = ();
	}

	calcNextArguments { |token, controls|
		var thisSetting, msgFunc;

		msgFunc = msgFuncs[token] ? #[];

		thisSetting = globalSettings.copy;


		synthSettings.at(token) !? {
			synthSettings.at(token).keysValuesDo { |key, val|
				if(val.isKindOf(Ref)) {
					// ref means that all subsequent synths on the same level will inherit the value
					current.put(key, val.dereference);
				} {
					thisSetting.put(key, val)
				}
			}
		};

		controls !? { thisSetting.putAll(controls) };
		thisSetting.parent = current;

		^thisSetting.use {
			msgFunc.valueEnvir(thisSetting)
		}

	}


}

