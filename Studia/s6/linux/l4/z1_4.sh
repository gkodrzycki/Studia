exec 3< wielowierszowy.txt
exec 4> logs.txt
bash ./czytaj 1 <&3 1>&4 &
bash ./czytaj 1.7 <&3 1>&4