


(
var selector = "quelle";
f = { |char|
	"// '%' \n".postf(char);
	"(\n~steno.%('%', { |in, controls|\n\n})\n);\n\n".postf(selector, char);


};

128.do { |x|
	var char = x.asAscii;
	if(char.isFileSafe and: { char.isSpace.not } and: { "!(){}[]".includes(char).not }) { f.(char) }
}
)


// operator
// default: binary
(
var selector = "operator";
f = { |char|
	"// '%' \n".postf(char);
	"(\n~steno.%('%', { |x, y, controls|\n\n}, 2)\n);\n\n".postf(selector, char);


};

128.do { |x|
	var char = x.asAscii;
	if(char.isFileSafe and: { char.isSpace.not } and: { "!(){}[]".includes(char).not }) { f.(char) }
}
)