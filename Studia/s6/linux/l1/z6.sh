zad1(){
	for package in $(pacman -Qq); do
	    if [ ! -d "/usr/share/doc/$package" ]; then
	        echo "$package"
	    fi
	done
}

zad2(){
	for dir in /usr/share/doc/*; do
		[ -d "$dir" ] || continue  # Sprawdź, czy to katalog
		package_name=$(basename "$dir")
		if ! pacman -Qq | grep -qi "^$package_name$"; then
			owner=$(pacman -Qo "$dir" 2>/dev/null | awk '{print $5}' | head -n1)
			if [ -n "$owner" ]; then
				echo "$package_name: $owner"
			else
				echo "$package_name: unknown owner"
			fi
		fi
	done
}

zad3(){
	for package in $(pacman -Qq); do
		if [ -d "/usr/share/doc/$package" ]; then
			if ! find "/usr/share/doc/$package/" -iname "changelog*" -print -quit | grep -q .; then
				echo "$package"
			fi
		fi
	done
}

zad4() {
	version_reg='[0-9]{,4}\.[0-9]{,4}\.[0-9]{,4}'
	date_reg='20[0-9]{2}[-./][0-9]{2}[-./][0-9]{2}'
	hash_reg='[a-f0-9]{8,32}|[A-F0-9]{8,32}'
	path='/usr/share/doc'

	for dir in $(ls -laA $path | awk '{print $9}'); do
		if pacman -Qq | grep "$dir" 2> /dev/null; then
			fullpath="$path/$dir"
			out=$(find $fullpath -regextype egrep -regex "$fullpath/.*[Cc]hange[Ll]og.*")
			if [[ ${#out} != 0 ]] ; then 
				for changelog in $out; do
					grep_out=$(grep -iE "$version_reg|$date_reg|$hash_reg" $changelog 2>/dev/null| nl | tail -n1 | awk '{print $1}')
					if [[ $grep_out -eq "1" ]]; then
						echo "$dir:$changelog"
					fi
				done
			fi
		fi
	done
}

zad5() {
	cat /usr/share/doc/bash/INTRO | grep -io bash | wc -l
}

"$1"