all:
	@mkdir -p build
	@rm -rf ./build/*
	@stack clean --docker
	@stack build --docker
	@cp `stack --docker path --local-install-root`/bin/bootstrap build
	@cd build && zip function.zip bootstrap && rm bootstrap && cd ..

update:
	aws lambda update-function-code --function-name test --zip-file fileb://build/function.zip
.PHONY: update