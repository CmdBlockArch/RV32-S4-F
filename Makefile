BUILD_DIR = ./build
PRJ = RV32S4F

V_FILE_GEN   = build/Core.sv
V_FILE_FINAL = build/Top.sv

idea:
	./mill -i mill.idea.GenIdea/idea

verilog:
	mkdir -p $(BUILD_DIR)
	./mill $(PRJ).runMain Elaborate --throw-on-first-error --full-stacktrace --target-dir $(BUILD_DIR)
	mv $(V_FILE_GEN) $(V_FILE_FINAL)
	sed -i '/firrtl_black_box_resource_files.f/, $$d' $(V_FILE_FINAL)

clean:
	rm -rf $(BUILD_DIR)

.PHONY: test verilog clean
