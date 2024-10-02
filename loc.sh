# Save the current cloc output, excluding the performance metrics line, to both stdout and a file
cloc --no3 --by-file *.cpp *.hpp | grep -v 'github.com/AlDanial/cloc' | tee current_cloc.txt

# If clock_diff.txt exists, compare it with current_cloc.txt
if [ -f .clock_diff.txt ]; then
  prev_total=$(grep 'SUM:' .clock_diff.txt | awk '{print $4}')
  curr_total=$(grep 'SUM:' current_cloc.txt | awk '{print $4}')
  added_lines=$((curr_total - prev_total))
  echo "Added lines of code: ${added_lines}"
else
  echo "No previous cloc data to compare."
fi

# Save the current cloc output as the previous output for the next run
mv current_cloc.txt .clock_diff.txt