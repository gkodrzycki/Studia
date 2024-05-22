exec 3< wielowierszowy.txt
bash ./czytaj 1 <&3 &
bash ./czytaj 1.7 <&3