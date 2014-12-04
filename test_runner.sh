#!/bin/bash

rm -rf test_output
mkdir test_output

red='\033[0;31m'
blue='\e[0;34m'
clr='\033[0m'
green='\e[0;32m'

echo -e "Starting HDC testrunner.";
for program in $(ls source_programs/*.d); do
  echo -e "${green}Testing $program ...${red}";

  basename=$(basename $program .d)
  program_diff=$basename.diff
  program_compiled=$basename.kernel

  ./dist/build/hdc/hdc < $program > test_output/$program_compiled

  if [ -f correct_output/$program_compiled ]; then
    echo -e "${clr}Generated diff for $program_compiled."
    diff test_output/$program_compiled correct_output/$program_compiled > test_output/$program_diff
  else
    echo -e "${red}Correct output not found for $program_compiled, skipping diff.${clr}"
  fi

  echo
done;

