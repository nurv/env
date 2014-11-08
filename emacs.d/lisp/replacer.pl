my $var;
{ local $/ = undef; local *FILE; open FILE, "<mic-paren.el"; $var = <FILE>; close FILE }


$var =~ s/\(, ([^\)]*)\)/,$1/g;

print $var;

