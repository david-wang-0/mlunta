build_checker_poly: create_dirs
	polyc -o build/mluntac-poly build.sml
	chmod u+x build/mluntac-poly

build_checker: create_dirs
	mlton -output build/mluntac-mlton src/checker.mlb
	chmod u+x build/mluntac-mlton

run_perf: build_perf
	cd build && ./perfing && mlprof perfing mlmon.out && cd ..

build_perf: bench.mlb
	mlton -output build/perfing -profile time bench.mlb

run_bench: build_bench_mlton
	./build/bench

build_and_run_bench: build_bench_mlton
	./build/bench

run_test: build_test
	@if [ 0 = $(shell ./build/tests | grep -c "Failed") ]; then\
		echo 'Tests succeeded';\
	else\
		echo 'Tests failed:';\
		./build/tests | grep "Failed";\
		exit 1;\
	fi

build_bench_poly: create_dirs
	polyc -o build/bench build_bench.sml

build_bench_mlton: create_dirs
	mlton -output build/bench bench.mlb

build_test: create_dirs
	polyc -o build/tests build_test.sml

create_dirs:
	mkdir -p build

clean_test:
	rm -f build/tests

clean:
	rm -f build/*
