exec 4> logs.txt
bash ./czytaj 1 < wielowierszowy.txt 1>&4 &
bash ./czytaj 1.7 < wielowierszowy.txt 1>&4