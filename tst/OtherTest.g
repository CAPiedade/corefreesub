gap> cfccs := CoreFreeConjugacyClassesSubgroups(SymmetricGroup(7));;
gap> ccs := Filtered(ConjugacyClassesSubgroups(SymmetricGroup(7)), i -> IsCoreFree(SymmetricGroup(7),i[1]));;
gap> Assert(1, Size(ccs)= Size(cfccs));;
gap> degs1 := Unique(List(cfccs, i -> Index(SymmetricGroup(7),i[1])));; Sort(degs1);; degs2 := Unique(List(ccs, i -> Index(SymmetricGroup(7),i[1])));; Sort(degs2);; 
gap> Assert(1, degs1 = degs2);;
