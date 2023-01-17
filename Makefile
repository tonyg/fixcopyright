__ignored__ := $(shell ./setup.sh)

PACKAGE_AND_COLLECT=fixcopyright

all: setup

clean:
	find . -name compiled -type d | xargs rm -rf
	find . -name '*.rkte' | xargs rm -rf

veryclean: clean
	rm -f licenses.json

setup:
	raco setup --check-pkg-deps --unused-pkg-deps $(PACKAGE_AND_COLLECT)

link:
	raco pkg install --link -n $(PACKAGE_AND_COLLECT) "$$(pwd)"

unlink:
	raco pkg remove $(PACKAGE_AND_COLLECT)

test: setup testonly

testonly:
	raco test -p $(PACKAGE_AND_COLLECT)

licenses.json:
	wget https://github.com/spdx/license-list-data/raw/main/json/licenses.json

fixcopyright:
	fixcopyright.rkt --preset-racket LGPL-3.0-or-later
