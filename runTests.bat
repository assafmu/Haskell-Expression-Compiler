ghc main.hs
main.exe < test.txt > result.py
python result.py > actual.txt
fc /a actual.txt testResult.txt
find /c /v "" result.py