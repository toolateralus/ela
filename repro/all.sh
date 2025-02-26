#!/bin/bash
trap "echo -e '\e[1;34m --testing cancelled-- \e[0m'; exit" SIGINT SIGTERM

fixed=()
unfixed=()
exec 3>&1 4>&2
exec 1>/dev/null 2>/dev/null
for file in *.ela; 
do
	base_name=$(basename "$file" .ela)
	if ela "$file"; then
		if ./"$base_name"; then
			fixed+=("$base_name")
		fi
		rm "$base_name"
	fi
done
for file in fixed/*.ela; 
do
	base_name=$(basename "$file" .ela)
	if ! ela "$file" ; then
		unfixed+=("$base_name")
	else
		if ! ./"$base_name"; then
			unfixed+=("$base_name")
		fi
		rm "$base_name"
	fi
done
exec 1>&3 2>&4
exec 3>&- 4>&-
echo -e "\e[0mFixed: \e[1;32m${fixed[@]}\e[0m"
echo -e "\e[0mUnfixed: \e[1;31m${unfixed[@]}\e[0m"

for test in "${fixed[@]}"; do
	read -p "Do you want to move the fixed repro '$test'? [y/n] " choice
	case "$choice" in 
		y|Y ) mv "$test".ela fixed/"$test".ela; echo "Moved $test";;
		n|N ) echo "Kept $test";;
		* ) echo "Invalid choice, kept $test";;
	esac
done
for test in "${unfixed[@]}"; do
	read -p "Do you want to move the unfixed repro '$test'? [y/n] " choice
	case "$choice" in 
		y|Y ) mv fixed/"$test".ela "$test".ela; echo "Moved $test";;
		n|N ) echo "Kept $test";;
		* ) echo "Invalid choice, kept $test";;
	esac
done