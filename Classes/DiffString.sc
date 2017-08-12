DiffString {

	var <>insertFunc, <>removeFunc, <>swapFunc, <>keepFunc;
	var <>beginFunc, <>returnFunc, <>testFunc = true;
	var <>verbose = false;
	classvar <>tempFilePath = "~/Desktop/";
	// state:
	var <prevTokens, <>tokenizer;


	*new { |insertFunc, removeFunc, swapFunc, keepFunc, beginFunc, returnFunc, testFunc|
		^super.newCopyArgs(insertFunc, removeFunc, swapFunc, keepFunc, beginFunc, returnFunc, testFunc ? true).init
	}

	init {
		prevTokens = [];
	}

	value { |newString, oldString, delimiter|
		var newTokens, oldTokens;
		newTokens = this.tokenize(newString, delimiter);
		if(oldString.notNil) { oldTokens = this.tokenize(oldString, delimiter) };
		^this.parse(newTokens, oldTokens)
	}

	parse { |newTokens, oldTokens|
		var diffString, returnValues;
		oldTokens = oldTokens ? prevTokens;
		if(verbose) { postf("old tokens:%\nnew tokens:%\n", oldTokens, newTokens) };
		if(oldTokens == newTokens) { ^this.prKeepAll(oldTokens) }; // shortcut. identity sometimes causes diff errors?
		diffString = this.calcDiff(newTokens, oldTokens);
		returnValues = this.prParseDiff(diffString);
		prevTokens = newTokens;
		^returnValues
	}

	// for now use this side-by-side parsing for clarity until a better way is found.
	calcDiff { |newTokens, oldTokens|
		var path1 = this.prWriteTempFile(oldTokens);
		var path2 = this.prWriteTempFile(newTokens);
		var cmdLine = "diff --minimal --side-by-side" + path1 + path2;
		var res = unixCmdGetStdOut(cmdLine);
		protect {
			// sometimes, the unixCmd can return fails silently, returning an empty string
			if(res.size == 0 and: { newTokens.size != oldTokens.size and: { oldTokens.size != 0 } }) {
				res = unixCmdGetStdOut(cmdLine); // try once again, usually it works.
				if(res.size == 0) { Error("commandline diff failed:" + cmdLine).throw };
			}
		} {
			File.delete(path1);
			File.delete(path2);
		};
		^res
	}

	tokenize { |string, delimiter|
		^if(tokenizer.notNil) {
			tokenizer.value(string, delimiter)
		} {
			if(delimiter.isNil) {
				string.as(Array)
			} {
				string.split(delimiter)
			}
		}
	}

	prKeepAll { |tokens|
		var res;
		if(verbose) { "keepin all tokens".postln };
		beginFunc.value;
		res = tokens.collect { |token, i|
			if(verbose) { postln("keeping" + token, i) };
			if(testFunc.value(i, Char.tab, token, token)) {
				keepFunc.value(token, i);
			};
		};
		^if(returnFunc.notNil) { returnFunc.value(res) } { res }
	}

	// analyze the diff output to call an appropriate function for each change
	prParseDiff { |diffString|
		var i = 0, index = 0, res;
		var lines = diffString.split(Char.nl);
		//"---------- lines: ".post; lines.postcs;
		lines = lines.drop(-1);
		beginFunc.value;
		res = lines.collect { |line|

			var oldToken, newToken, tab = Char.tab;
			var operator, opIndex, firstTabIndex, returnValue;

			if(line[line.lastIndex] == $|) { line = line.drop(-1).add($<); }; // fix for alternative removal syntax

			opIndex = line.findBackwards("\t");
			firstTabIndex = line.find("\t");
			if(opIndex.isNil or: firstTabIndex.isNil) { this.prError };

			opIndex = opIndex - 1;
			operator = line[opIndex];
			oldToken = line[0 .. firstTabIndex - 1];
			newToken = line[opIndex + 2 ..];

			if(operator == tab) { if(line.last == $<) { operator = $< } };
			if(oldToken == tab.asString) { operator = $> }; // assumption
			if(verbose) {
				postcs(line);
				postf("operator: %, index: %\noldToken: %\nnewToken: %\n", operator.cs, i, oldToken.cs, newToken.cs);
			};

			returnValue = switch(operator,
				// keeping
				tab, {
					if(verbose) { postln("keeping" + oldToken, i) };
					index = index + 1;
					if(testFunc.value(i, operator, oldToken, newToken)) {
						keepFunc.value(oldToken, i);
					};
				},
				// removing
				$<, {
					if(verbose) { postln("removing" + oldToken) };
					if(testFunc.value(i, operator, oldToken, "")) { // newToken should be nothing when removing.
						removeFunc.value(oldToken, i)
					};
				},
				// inserting
				$>, {
					if(verbose) { postln("inserting" + newToken) };
					index = index + 1;
					if(testFunc.value(i, operator, "", newToken)) { // oldToken should be nothing when inserting.
						insertFunc.value(newToken, i);
					};
				},
				// replacing
				$|, {
					if(verbose) { postln("replacing" + oldToken + "with" + newToken) };
					index = index + 1;
					if(testFunc.value(i, operator, oldToken, newToken)) {
						swapFunc.value(newToken, i, oldToken)
					};
				},
				$\\, {  // incomplete line
					if(verbose) { postln("******** newline missing" + oldToken, i) };
					index = index + 1;
					/*
					if(testFunc.value(i, operator, oldToken, newToken)) {
					keepFunc.value(oldToken, i);
					};
					*/
				},
				$/, { // incomplete line
					if(verbose) { postln("******** newline missing" + oldToken, i) };
					index = index + 1;
					/*
					if(testFunc.value(i, operator, oldToken, newToken)) {
					keepFunc.value(oldToken, i);
					};
					*/
				},
				{
					"Operator % failed. Current state at index: %:\noldToken: %\nnewToken: %\n"
					.postf(operator, oldToken, newToken, i);
					this.prError
				}
			);
			i = index;
			returnValue
		};
		if(verbose) { "finished parse".postln };
		^if(returnFunc.notNil) { returnFunc.value(res) } { res }
	}

	prWriteTempFile { |tokens|
		var path = tempFilePath +/+ "sc_diff_temp_" ++ UniqueID.next;
		path = path.standardizePath;
		try {
			File.use(path, "wb", { |file|
				if(tokens.size == 0) {
					file.write(Char.nl);
				} {
					tokens.do { |token|
						file.write(token.asString ++ Char.nl);
					}
				};
			});
		} { |error|
			"DiffString couldn't write file. Perhaps you don't have permissions for the path? "
			"The current path is: %. You can change it in DiffString.tempFilePath = ...\n".postf(path.absolutePath);
			error.throw;
		};
		^path
	}

	prError {
		Error("Diff: diff returned incompatible format").throw
	}


}


// trying without writing files.

DiffString2 : DiffString {

	prepareString { |tokens|
		var res = String.new;
		tokens.do { |token| res = res ++ token ++ Char.nl };
		^res.postcs
	}


	calcDiff { |newTokens, oldTokens|
		var string1 = this.prepareString(oldTokens);
		var string2 = this.prepareString(newTokens);
		^unixCmdGetStdOut(
			// seems to be incorrect ...
			format(
				"mkfifo ./p; diff --minimal --side-by-side - p < % & echo % > p",
				string1.quote,
				string2.quote
			)
		)
	}
}


