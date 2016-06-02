test: src/sonnex.rules src/Type/Rule.hs src/Parser/Parser.hs src/Generator/PHP.intro src/Generator/PHP.outro src/Generator/PHP.hs src/Main.hs
	cabal build
	dist/build/Sonnex/Sonnex > test/sonnex.php
	cat test/frenchwords.txt | php test/testsonnex.php > test/out.txt
