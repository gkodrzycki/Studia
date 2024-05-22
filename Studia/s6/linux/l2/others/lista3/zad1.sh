OIFS=$IFS
IFS=$'\n'

music_array=()
i=0
for music in $(find . -name "*.mp3"); do
    echo -n $i
    mp3info -p ") %l (%a): %t\n" $music
    music_array[i]=$music
    i=$(($i+1))
    echo $music
done

echo -n "Choose a number to play> "
while read -r command ; do
    if (( $command < $i )) && (( $command >= 0 )); then
        mplayer ${music_array[$command]}
        echo -n "Choose a number to play> "
    else 
        echo "wrong answer"
    fi
done

clear
