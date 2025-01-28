BUILD_DIR = ./build
PRJ = RV32S4F
V_FILE = build/Top.sv

idea:
	./mill -i mill.idea.GenIdea/idea

verilog:
	mkdir -p $(BUILD_DIR)
	./mill $(PRJ).runMain Elaborate --throw-on-first-error --full-stacktrace --target-dir $(BUILD_DIR)
	sed -i '/firrtl_black_box_resource_files.f/, $$d' $(V_FILE)

clean:
	rm -rf $(BUILD_DIR)

.PHONY: test verilog clean
