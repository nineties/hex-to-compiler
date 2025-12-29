# zero-to-compiler -
# Copyright (C) 2025 nineties

default: pforth

pforth: pforth.xxd
	xxd -r -c 8 $< > pforth
	chmod +x pforth

.PHONY: clean test
clean:
	rm -f pforth

test: pforth
