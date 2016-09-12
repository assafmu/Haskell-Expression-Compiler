ghc main.hs
main.exe < input.txt > result.py
python result.py > actual.txt
fc /a actual.txt expected.txt
find /c /v "" result.py
del *.hi
del *.o