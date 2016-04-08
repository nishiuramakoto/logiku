
test:
	cd ../.. && time cabal test 2>&1 | sed -r 's/\x1b\[[^@-~]*[@-~]//g'
	export STATUS=$$?
	echo $$STATUS
	cat /tmp/time.log
	return $$STATUS
