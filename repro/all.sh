#!/bin/bash
trap "echo -e '\e[1;34m --testing cancelled-- \e[0m'; exit" SIGINT SIGTERM

passing=()
failing=()
exec 3>&1 4>&2
exec 1>/dev/null 2>/dev/null
for file in *.ela; 
do
	base_name=$(basename "$file" .ela)
	if ela "$file" && ./"$base_name"; then
		rm "$base_name"
		passing+=("$base_name")
	else
		failing+=("$base_name")
	fi
done
exec 1>&3 2>&4
exec 3>&- 4>&-
echo -e "\e[1;32mPassed: ${passing[@]}\e[0m"
echo -e "\e[1;31mFailed: ${failing[@]}\e[0m"
echo -e "\e[1;31m# of Failed: ${#failing[@]}\e[0m"

for test in "${passing[@]}"; do
	read -p "Do you want to prune the passing test '$test'? [y/n] " choice
	case "$choice" in 
		y|Y ) rm "$test".ela; echo "Pruned $test";;
		n|N ) echo "Kept $test";;
		* ) echo "Invalid choice, kept $test";;
	esac
done