# corefreesub, chapter 3
#
# DO NOT EDIT THIS FILE - EDIT EXAMPLES IN THE SOURCE INSTEAD!
#
# This file has been generated by AutoDoc. It contains examples extracted from
# the package documentation. Each example is preceded by a comment which gives
# the name of a GAPDoc XML file and a line range from which the example were
# taken. Note that the XML file in turn may have been generated by AutoDoc
# from some other input.
#
gap> START_TEST("corefreesub02.tst");

# doc/_Chapter_Faithful_Transitive_Permutation_Representations.xml:23-34
gap> sp := SymplecticGroup(4,2);;
gap> CoreFreeDegrees(sp);
[ 6, 10, 12, 15, 20, 30, 36, 40, 45, 60, 72, 80, 90, 120, 144, 180, 240, 360, 
  720 ]
gap> ftprs := FaithfulTransitivePermutationRepresentations(sp);; 
gap> Size(ftprs);
19
gap> all_ftprs := FaithfulTransitivePermutationRepresentations(sp,true);; 
gap> Size(all_ftprs);
54

# doc/_Chapter_Faithful_Transitive_Permutation_Representations.xml:58-65
gap> sp := SymplecticGroup(4,2);;
gap> min_ftpr := MinimalFaithfulTransitivePermutationRepresentation(sp);
CompositionMapping( <action epimorphism>, <action isomorphism> )
gap> min_ftprs := MinimalFaithfulTransitivePermutationRepresentation(sp,true);
[ CompositionMapping( <action epimorphism>, <action isomorphism> ), 
  CompositionMapping( <action epimorphism>, <action isomorphism> ) ]

# doc/_Chapter_Faithful_Transitive_Permutation_Representations.xml:78-84
gap> sp := SymplecticGroup(4,2);; g:=SimpleGroup("PSL",3,5);;
gap> MinimalFaithfulTransitivePermutationDegree(sp);
6
gap> MinimalFaithfulTransitivePermutationDegree(g);
31

# doc/_Chapter_Faithful_Transitive_Permutation_Representations.xml:108-116
gap> sp := SymplecticGroup(4,2);;
gap> FaithfulTransitivePermutationRepresentationsOfDegree(sp,10);
CompositionMapping( <action epimorphism>, <action isomorphism> )
gap> FaithfulTransitivePermutationRepresentationsOfDegree(sp,20, true);
[ CompositionMapping( <action epimorphism>, <action isomorphism> ), 
  CompositionMapping( <action epimorphism>, <action isomorphism> ), 
  CompositionMapping( <action epimorphism>, <action isomorphism> ) ]

#
gap> STOP_TEST("corefreesub02.tst", 1);
