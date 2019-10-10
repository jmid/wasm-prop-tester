clear
reset
set key off
set border 3

# Add a vertical dotted line at x=0 to show centre (mean) of distribution.
set yzeroaxis

# Each bar is half the (visual) width of its x-range.
set boxwidth 0.8 absolute
set style fill solid 1.0 noborder

bin_width = 0.1;

bin_number(x) = floor(x/bin_width)

rounded(x) = bin_width * ( bin_number(x) + 0.5 )

#plot '../ocaml/test/test_1556799786.csv' using (rounded($1)):(1) smooth frequency with boxes
#plot '../ocaml/test/test_1556802457.csv' using (rounded($1)):(1) smooth frequency with boxes
plot '../ocaml/test/test_1556802625.csv' using (rounded($1)):(1) smooth frequency with boxes

#plot '../ocaml/test/test_1556801646.csv' using (rounded($1)):(1) smooth frequency with boxes