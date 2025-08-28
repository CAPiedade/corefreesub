gap> cfccs := CoreFreeConjugacyClassesSubgroups(SymmetricGroup(7));;
gap> ccs := Filtered(ConjugacyClassesSubgroups(SymmetricGroup(7)), i -> IsCoreFree(SymmetricGroup(7),i[1]));;
gap> Size(ccs)= Size(cfccs);
true
gap> degs1 := Unique(List(cfccs, i -> Index(SymmetricGroup(7),i[1])));; Sort(degs1);; degs2 := Unique(List(ccs, i -> Index(SymmetricGroup(7),i[1])));; Sort(degs2);; 
gap> degs1 = degs2;
true
gap> cfccs := CoreFreeConjugacyClassesSubgroupsCyclicExtension(SymmetricGroup(7));;
gap> ccs := Filtered(ConjugacyClassesSubgroups(SymmetricGroup(7)), i -> IsCoreFree(SymmetricGroup(7),i[1]));;
gap> Size(ccs)= Size(cfccs);
true
gap> cfccs := CoreFreeConjugacyClassesSubgroupsViaRadical(SymmetricGroup(8));;
gap> ccs := Filtered(ConjugacyClassesSubgroups(SymmetricGroup(8)), i -> IsCoreFree(SymmetricGroup(8),i[1]));;
gap> Size(ccs)= Size(cfccs);
true
gap> d := DirectProduct(SymmetricGroup(5),SymmetricGroup(6));; cfccs := CoreFreeConjugacyClassesSubgroups(d);;
gap> ccs := Filtered(ConjugacyClassesSubgroups(d), i -> IsCoreFree(d,i[1]));;
gap> Size(ccs)= Size(cfccs);
true
gap> g := PcGroupCode(105641121221273607348650803, 2000);; cfccs := CoreFreeConjugacyClassesSubgroupsOfSolvableGroup(g);;
gap> ccs := Filtered(ConjugacyClassesSubgroups(g), i -> IsCoreFree(g,i[1]));;
gap> Size(ccs)= Size(cfccs);
true
gap> subtrivialfitting := Filtered(SubgroupsTrivialFitting(SymmetricGroup(7)), k -> IsCoreFree(SymmetricGroup(7),k));;
gap> cfsubtrivialfitting := CoreFreeSubgroupsTrivialFitting(SymmetricGroup(7));;
gap> Size(subtrivialfitting) = Size(cfsubtrivialfitting);
true
