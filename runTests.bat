ghc main.hs
main.exe < test.txt > result.py
python result.py > actual.txt
fc /a actual.txt testResult.txt
del *.hi
del *.o