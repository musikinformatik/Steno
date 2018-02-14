

ProbingStenoTest : UnitTest {

	var steno, bus, server;

	setUp {
		server = Server.default;
		this.bootServer(server);
		server.sync;
		bus = Bus.control(server, 1);
		steno = Steno.new(1, server:server);
		steno.bus = Bus.audio(server, 1); // don't listen, use private bus

	}


	test_serial_signal_path {

		var func, envir, dt;

		dt = 0.3;

		steno.fadeTime = 0;

		envir = (a:1, b: 3, c:7, d:11);

		envir.keysValuesDo { |key, val|
			steno.quelle(key, { |in, e| DC.ar(val) * e[\gate] });
		};

		steno.operator('*', { |a, b| a * b }, 2);
		steno.operator('+', { |a, b| a + b }, 2);

		steno.quelle('>', { |in| Out.kr(bus, A2K.kr(in)) });
		steno.quelle('1', { |in| DC.ar(1) });

		server.sync;

		func = { |code, result, message|
			var busValue;
			steno.value("");
			server.setControlBusValue(bus.index, 0);

			dt.wait;

			steno.value(code);

			dt.wait;

			busValue = server.getControlBusValue(bus.index);
			this.assertEquals(busValue, result, message);
		};

		envir.use {
			func.("abcd>", ~a + ~b + ~c + ~d, "serial signals should add up");
			func.("[abcd]>", ~a + ~b + ~c + ~d, "parallel signals should add up");
			func.("{abcd+++}>", ~a + ~b + ~c + ~d, "operator signals should be combined");

		};

		// filters
		envir.keysValuesDo { |key, val|
			steno.filter(key, { |in, e| in * DC.ar(val) * e[\gate] });
		};


		1.wait;

		envir.use {
			func.("1a>", ~a, "tesing filter identity");
			steno.dumpStructure;
		};

	}

	tearDown {
		server.quit
	}


}



