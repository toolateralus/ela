#!/usr/bin/env python3
import re

def parse_cloc(filename):
  files = {}
  with open(filename) as f:
    for line in f:
      # Match lines like: src/emit.cpp   384   121   2123
      m = re.match(r'^(\S+)\s+(\d+)\s+(\d+)\s+(\d+)', line)
      if m:
        files[m.group(1)] = int(m.group(4))
  return files

def print_table(curr_file, prev_file):
  prev = parse_cloc(prev_file) if prev_file else {}
  curr = []
  with open(curr_file) as f:
    for line in f:
      m = re.match(r'^(\S+)\s+(\d+)\s+(\d+)\s+(\d+)', line)
      if m:
        file, blank, comment, code = m.groups()
        code = int(code)
        delta = code - prev.get(file, 0)
        curr.append((file, int(blank), int(comment), code, delta))
      elif line.strip().startswith('SUM:'):
        sum_code = int(line.strip().split()[-1])
        prev_sum = 0
        if prev_file:
          with open(prev_file) as pf:
            for pline in pf:
              if pline.strip().startswith('SUM:'):
                prev_sum = int(pline.strip().split()[-1])
                break
        sum_delta = sum_code - prev_sum
        curr.append(('TOTAL', '', '', sum_code, sum_delta))
      elif line.strip() == '' or line.startswith('---') or line.startswith('File'):
        curr.append((line.rstrip(), '', '', '', ''))
      else:
        curr.append((line.rstrip(), '', '', '', ''))

  # Print table
  for row in curr:
    if isinstance(row[1], int) or row[0] == 'TOTAL':
      print(f"{row[0]:<45} {row[1]:>8} {row[2]:>10} {row[3]:>8} {row[4]:>+8}")
    else:
      print(row[0])

if __name__ == "__main__":
  import os
  curr = "current_cloc.txt"
  prev = ".clock_diff.txt" if os.path.exists(".clock_diff.txt") else None
  # Run cloc and save output
  os.system('cloc --no3 --by-file src/*.cpp include/*.hpp | grep -v "github.com/AlDanial/cloc" > current_cloc.txt')
  print_table(curr, prev)
  # Save current as previous for next run
  os.replace("current_cloc.txt", ".clock_diff.txt")