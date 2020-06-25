COPY .\CompileLater.hs .\src\CompileToTest.hs
CD .\src
ghc -fno-code .\CompileToTest.hs -Wall