/*
syntax colorisation for a difference between lines.
*/

(
var str =
"
abc
axc
xxxy
xkxxy
klakxmxcvay
klixmmycvay
klymix
abkly
";
var lines = str.split(Char.nl).collect { |line| line.as(Array) };
var diff, lineIndex;
var indices = [];
diff = DiffString(
	beginFunc: {
		var i = indices[lineIndex - 1].copy ?? { { lineIndex } ! lines[lineIndex].size };
		indices = indices.add(i);
	},
	insertFunc: { |token, i| indices[lineIndex] = indices[lineIndex].insert(i, lineIndex) },
	swapFunc: { |token, i| indices[lineIndex] = indices[lineIndex].put(i, lineIndex) },
	removeFunc: { |token, i| indices[lineIndex].removeAt(i); },
	returnFunc: { |token, i| indices[lineIndex].postcs; }
);
//diff.verbose = true;
lines.doAdjacentPairs { |a, b, i|
	lineIndex = i;
	a.join.post; " -> ".post; b.join.post; ":".postln;
	diff.parse(b, a);

};

indices = indices.addFirst([]);

~lines = lines;
~indices = indices;
~maxLineSize = 25; //lines.maxItem(_.size).size;
~update.value(str);
~lines;
~string = str;
"";
)


(
~lines.do { |x| x.join.postcs };"";
~indices.do(_.postln);"";
)

(
var colors = { Color.rand(0.3, 1.0) } ! (~lines.size + 1);
var views;
var selection;
w = Window().layout_(
	VLayout(*{ |j|
		var line = { StaticText.new } ! ~maxLineSize;
		views = views.add(line);
		HLayout(*line.collect { |x| [x, align:\left] })
	} ! ~lines.size)
);
~update = {
	views.do { |line, j|
		line.do { |v, i|
			var ci = try { ~indices[j][i] };
			var str = try { ~lines[j][i] };
			str !? { v.string_(str.asString) };
			if(ci.notNil) { v.stringColor_(colors[ci]) } { v.stringColor_(Color.white) };
			//if(selection == j) { v.background = Color.grey } { v.background = Color.clear };
			v.font = Font("Menlo", 23);
			v.mouseMoveAction = { colors[j] =  colors[j].vary; ~update.value };
			//v.mouseDownAction = { selection = j; ~update.value };
			v.maxWidth_(30);
		}
	};
};
~update.value;
w.background = Color.black;
w.front;
w.onClose = { ~update = nil };
~colors = colors;
"";
)
