



// add unicode support is easy.
t = Steno.new;
t.diff.tokenizer = { |str| str.asStrang }; // for some reason we need this.
t.quelle('⚀', { SinOsc.ar * 0.1 });
t.verbosity = 2;
t.value("⚀⚂⚁".asStrang);

t.quelle('⚂', { SinOsc.ar(652) * 0.1 });
t.quelle('⚁', { SinOsc.ar(761) * 0.1 });
t.push;

--⚂⚁

// testing why the tokenizer needs to be changed.

'Ì' == 'Ì';
t.rawCmdLine.class
t.cmdLine.class
t.preProcess = false;
t.prevTokens;


// this works.
d = DiffString(insertFunc: { |new| "Please say hello to: %\n".postf(new) });
d.value("⚀⚂⚁".asStrang);
d.value("⚀⚂⚁".asStrang);

d.prevTokens;
t.value("⚀⚂⚁".asStrang);
t.prevTokens;
d = t.diff;
d.value("⚀⚂⚁".asStrang);
d.value("");



// ??
