BUILD_DIR = ./build

PRJ = RV32S4F

idea:
	./mill -i mill.idea.GenIdea/idea

verilog:
	mkdir -p $(BUILD_DIR)
	./mill $(PRJ).runMain Elaborate --target-dir $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)

.PHONY: test verilog clean
