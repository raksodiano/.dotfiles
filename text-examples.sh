#!/bin/bash

output="figlet_tests.txt"
echo "Figlet font tests" > $output
echo "Date: $(date)" >> $output
echo "==================================" >> $output

figlist | awk '/Figlet fonts in this directory:/,/Figlet control files in this directory:/' | grep -vE 'Figlet|fonts|in|this|directory|control|files' | while read font; do
    [ -z "$font" ] && continue
    echo "Font: $font" >> $output
    figlet -f "$font" "TEST" >> $output 2>/dev/null
    echo "" >> $output
done

echo "Tests completed. Results in $output"