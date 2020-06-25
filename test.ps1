Copy-Item .\CompileLater.hs .\src\CompileToTest.hs
Set-Location .\src
ghc -fno-code CompileToTest.hs