#!/bin/bash

rm -rf test_output
mkdir test_output

for program in $(ls source_programs/*.d); do
  echo "Testing $program ...";

  basename=$(basename $program .d)
  program_diff=$basename.diff
  program_compiled=$basename.kernel

  ./dist/build/hdc/hdc < $program > test_output/$program_compiled
  diff test_output/$program_compiled correct_output/$program_compiled > test_output/$program_diff
done;

