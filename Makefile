all:

clean:
	rm -f licenses.json

licenses.json:
	wget https://github.com/spdx/license-list-data/raw/main/json/licenses.json
