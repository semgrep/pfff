#!/bin/bash

# Expected input: first arg is the file with menhir error messages
#                 second arg is the max allowed conflicts
#                 third arg is the language
FILE=$1
num=$2
lang=$3
generated_ml_file=$4
generated_mli_file=$5
INSTR="To bypass this, manually set NUM_PERMITTED_CONFLICTS in pfff/lang_$lang/parsing/Makefile"

# Parse conflicts output
# Expect "Warning: [num] states have shift/reduce conflicts"
# and/or "Warning: [num] states have reduce/reduce conflicts"
# Split by space to get [num]

resultsr=`grep -i 'have shift/reduce conflicts' $FILE | cut -d ' ' -f 2` 
resultrr=`grep -i 'have reduce/reduce conflicts' $FILE | cut -d ' ' -f 2` 
result=$((resultsr+resultrr))
MSG="Error: menhir found $result conflicts when $num are permitted. See pfff/lang_$lang/parsing/menhir_out.log for details. ($INSTR)"

if (( $result > $num )); then
   echo $MSG 1>&2
   rm -f $generated_ml_file $generated_mli_file
   exit 126
fi
