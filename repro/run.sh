if ela $1.ela; then
	./$1
	run_status=$?
	rm $1
	exit $run_status
else
	exit 1
fi