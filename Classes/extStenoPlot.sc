+ Steno {

	// this represents the structure of the actual implementation
	// not complete

	plotStructure { |title = "untitled"|
		var window, run = true;
		var in, out, prevNode, curNode;
		window = Window(title);
		window.background = Color.black;
		window.onClose = { run = false };
		window.drawFunc = {
			var prevNodes = Array.newClear(busIndices.size);
			synthList.do { |synth, i|
				var nodeSize = 20;
				var args = argList.at(i);
				var name = this.removePrefix(synth.defName);
				var mul = Point(
					window.bounds.width - (nodeSize * 2) / busIndices.size,
					window.bounds.height - (nodeSize * 2) / synthList.size
				);
				if(args.isEmpty.not and: { name != ')' }) {
					in = busIndices.indexOf(args[1]);
					//in = busIndices.indexOf(args[5]);
					out = busIndices.indexOf(args[3]);
				} {
					out = out ? 0;
					in = in ? in;
				};
				prevNode = prevNodes[in];
				curNode = Point(out + 1, i + 1) * mul;
				prevNodes[out] = curNode;
				Pen.color = Color.white;
				Pen.moveTo(curNode);
				Pen.addOval(Rect.aboutPoint(curNode, nodeSize, nodeSize));
				Pen.moveTo(curNode);
				if(name != '?') { Pen.stringAtPoint(name, curNode, Font.sansSerif(nodeSize)) };
				Pen.moveTo(curNode);
				prevNode !? { Pen.lineTo(prevNode) };
			};
			Pen.stroke;
		};
		{ while { run } { window.refresh; 0.05.wait; } }.fork(AppClock)
		^window.front;
	}

	dotStructure { |title, attributes, labelAttributes|
		attributes = attributes ?? { "rankdir=LR;\nfontname=Courier;\nlabel=\"\n\n%\"".format(cmdLine) };
		labelAttributes = labelAttributes ?? { "fontname=Courier" };
		^this.cmdLine.stenoDotStructure(title ? cmdLine, attributes, variables.keys, operators, labelAttributes)
	}

}