gap> cfccs := CoreFreeConjugacyClassesSubgroups(SymmetricGroup(7));;
gap> ccs := Filtered(ConjugacyClassesSubgroups(SymmetricGroup(7)), i -> IsCoreFree(SymmetricGroup(7),i[1]));;
gap> Assert(1, Size(ccs)= Size(cfccs));;
