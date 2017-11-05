(
t = Steno.push;
t.verbosity = 0;
t.quant = nil;
s.boot;
t.setGlobal(\freq, 700, \fadeTime, 0.04);
)


// define some synths and some filters
(
t.quelle(\a, { |in| Impulse.ar(Rand(3, 7) ! in.numChannels, 0, 0.3) * EnvGate.new });
t.quelle(\r, { |in| PinkNoise.ar(0.1 ! in.numChannels) * EnvGate.new });

t.filter(\b, { |in| Ringz.ar(in, Rand(700, 800), 0.1) });
t.filter(\c, { |in| DelayL.ar(in, 0.1, SinOsc.kr(Rand(0.2, 1.0)).range(0, 0.1)) });

t.quelle('=', { SoundIn.ar([0, 1]) });
t.quelle(\m, { SinOsc.ar(Rand(300, 2130)) });
t.quelle(\u, { Blip.ar(Rand(1, 115, 300)) * 3 });
t.quelle(\x, { SinOsc.ar(220) * 0.7 });
t.quelle(\y, { SinOsc.ar(330) * 0.6 });
t.quelle(\z, { SinOsc.ar(\freq.kr(440) * Rand(1, 1.3)) * 0.5 });

t.filter(\h, { |input| LFPulse.kr(Rand(1, 1.2), 0, 0.3) * input });
t.filter(\i, { |input| LFPulse.kr(Rand(3, 6), 0, 0.5) * input });
t.filter(\j, { |input| LFPulse.kr(Rand(0.4, 0.7), 0, 0.1) * input });
t.filter(\f, { |input| FreeVerb.ar(input, 2.8, 0.5, 0.5)  });
t.filter(\g, { |input| CombL.ar(input, 0.1, 0.1, 1.4) + input });
t.filter(\t, { |input, ctrl| CombL.ar(Dust2.ar(30) * ctrl[\gate], 0.1, Rand(0.01, 0.1), 4)  });
);


// now you can write things like:

--mmmmm
--mmhmhmhm
--mmh mh mh mh
--[mmhf mh mhf mh]
--[mmhf mh mhf mh]g
--![mmhf mh mhf mh]g

fork { loop { 0.2.wait; t.("muxyzxyz".scramble) } };

--[(mhg)(mhf)(mh)]g
--
--mxzy
--[mihf mif]
--
--[(ufhgij)(ufhgi)(=fhgi)]fh[(igf)(igf)]
--ugh uhg ughf uug

--aaagg
--raagg
--[([auu])f([auuu]h)]
-- (x)(a)

(--
arugrh
rarhari
yh
)

t.encyclopedia


// violent tests
t.quant = nil;
t.verbosity = 0;
Tdef(\x, { loop { 0.1.wait; t.("xryuzthij".scramble) } }).play;
Tdef(\x, { loop { 0.1.wait; try { t.("xryurrzghijtabh[]ij[]".scramble) } } }).play;
Tdef(\x, { var st = "xryurrjjuummzg()()hi   jtabh[]ij[]".scramble; loop { 0.3.wait; st = st.copy.swap(st.size.rand, st.size.rand).postln; t.cmdLine.postln; try { t.(st) } } }).play;
Tdef(\x).stop;
--!

~twist.synthList;
