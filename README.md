# Steno
A little language for spinning long strings of synths

Steno, a little concatenative livecoding language embedded in SuperCollider, makes code for combining synth graphs very short, so that writing endless chains is like writing a single long word. A word is a program. A letter is a synth.

Programs look like this: --ab[(ab)(ab)(ab)]c

![ScreenShot](graph02.svg)

Semantics:

- For live coding it follows a *dual language approach*: You can rewrite both the steno program and its sclang interpretation at runtime. So the meaning of each letter, the synth def, can be changed and the corresponding program changes accordingly.
- Programs are composed of concurrently running processes. In other words, the whole program has no explicit temporal semantics, instead each component (letter) stands for its own temporal unfolding.
- Only the part of the program that corresponds to code that was rewritten is changed. Thus, elements are kept, replaced, removed or inserted accoding to the difference between the old and the new program.


Steno has a minimal syntax:

- one letter is one synth: abc
- square brackets: nested structure is parallel: [ab]c
- round parentheses: nested structure is serial: [(ab)(ab)]
- force a full replacement on eval: !abc
- line ending comments are ignored: abc // this is a comment
- variable names are optional and can be declared explicitly, see: -declareVariables
