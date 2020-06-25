#!bash

cp ./CompileLater.hs ./src/CompileToTest.hs
cd ./src
ghc -fno-code CompileToTest -Wall