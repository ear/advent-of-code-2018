head -n 1 input.txt
cat input.txt | awk 'NR>1{print}' | tr ' ' '\n' | lam - -F '10.10' - - -f '4' - 2>/dev/null
