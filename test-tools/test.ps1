Copy-Item .\NotYet.hs ..\src\CompileToTest.hs
Set-Location ..\src
ghc -fno-code CompileToTest.hs