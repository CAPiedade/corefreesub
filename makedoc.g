#
# corefreesub: A GAP Package for calculating the core-free subgroups and their faithful transitive permutation representations
#
# This file is a script which compiles the package manual.
#
if fail = LoadPackage("AutoDoc") then
    Error("AutoDoc version 2018.02.14 or newer is required.");
fi;

AutoDoc( rec( autodoc := true, maketest := true));#, extract_examples := true ) );

QUIT;
