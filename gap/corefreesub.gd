########################################################################
##
##  CoreFreeSub package
##
##  Copyright 2022
##    A GAP Package for calculating the core-free subgroups and their faithful transitive permutation representations
##    
##    
##
## Licensed under the GPL 3 or later.
##
########################################################################
# 
DeclareInfoClass( "InfoCoreFreeSub" );
SetInfoLevel( InfoCoreFreeSub, 1 );
#! @Chapter Introduction
#!
#! corefreesub is a package which calculates the core-free subgroups and their faithful transitive permutation representations
#! 
#!
#! @Chapter Obtaining Core-Free Subgroups
#
#! @Section Core-Free Subgroups
#! 
#! A core-free subgroup is a subgroup in which its (normal) core is trivial.
#!
#! @Arguments G, H
#! @Returns a boolean
#! @Description
#! Given a group <A>G</A> and one of its subgroups <A>H</A>, it returns whether <A>H</A> is core-free in <A>G</A>.
#! @BeginExampleSession
#! gap> G := SymmetricGroup(4);; H := Subgroup(G, [(1,3)(2,4)]);;
#! gap> Core(G,H);
#! Group(())
#! gap> IsCoreFree(G,H);
#! true
#! gap> H := Subgroup(G, [(1,4)(2,3), (1,3)(2,4)]);;
#! gap> IsCoreFree(G,H);
#! false
#! gap> Core(G,H); # H is a normal subgroup of G, hence it does not have a trivial core
#! Group([ (1,4)(2,3), (1,3)(2,4) ])
#! @EndExampleSession
DeclareGlobalFunction( "IsCoreFree" );

#!
#! 
#! @Arguments G
#! @Returns a list
#! @Description
#!  Returns a list of all conjugacy classes of core-free subgroups of <A>G</A>
#! @BeginExampleSession
#! gap> G := SymmetricGroup(4);; dh := DihedralGroup(10);;
#! gap> CoreFreeConjugacyClassesSubgroups(G);
#! [ Group( () )^G, Group( [ (1,3)(2,4) ] )^G, Group( [ (3,4) ] )^G, Group( [ (2,4,3) ] )^G, Group( [ (3,4), (1,2)(3,4) ] )^G, Group( [ (1,3,2,4), (1,2)(3,4) ] )^G, Group( [ (3,4), (2,4,3) ] )^G ]
#! gap> CoreFreeConjugacyClassesSubgroups(dh);
#! [ Group( <identity> of ... )^G, Group( [ f1 ] )^G ] 
#!@EndExampleSession
DeclareGlobalFunction( "CoreFreeConjugacyClassesSubgroups" );

#!
#! @Arguments G
#! @Returns a list
#! @Description
#!  Returns a list of all core-free subgroups of <A>G</A>
#! @BeginExampleSession
#! gap> G := SymmetricGroup(4);; dh := DihedralGroup(10);;
#! gap> AllCoreFreeSubgroups(G);
#! [ Group(()), Group([ (1,3)(2,4) ]), Group([ (1,4)(2,3) ]), Group([ (1,2)(3,4) ]), Group([ (3,4) ]), Group([ (2,4) ]), Group([ (2,3) ]), Group([ (1,4) ]), Group([ (1,3) ]), Group([ (1,2) ]), Group([ (2,4,3) ]), Group([ (1,3,2) ]), Group([ (1,3,4) ]), Group([ (1,4,2) ]), Group([ (3,4), (1,2)(3,4) ]), Group([ (2,4), (1,3)(2,4) ]), Group([ (2,3), (1,4)(2,3) ]), Group([ (1,3,2,4), (1,2)(3,4) ]), Group([ (1,2,3,4), (1,3)(2,4) ]), Group([ (1,2,4,3), (1,4)(2,3) ]), Group([ (3,4), (2,4,3) ]), Group([ (1,3), (1,3,2) ]), Group([ (1,3), (1,3,4) ]), Group([ (1,4), (1,4,2) ]) ]
#! gap> AllCoreFreeSubgroups(dh);
#! [ Group([  ]), Group([ f1 ]), Group([ f1*f2^2 ]), Group([ f1*f2^4 ]), Group([ f1*f2 ]), Group([ f1*f2^3 ]) ]
#!@EndExampleSession
DeclareGlobalFunction( "AllCoreFreeSubgroups" );

#! @Section Degrees of Core-Free subgroups
#! 
#!
#! @Arguments G
#! @Returns a list
#! @Description
#!  Returns a list of all possible degrees of faithful transitive permutation representations of <A>G</A>. The degrees of a faithful transitive permutation representation of <A>G</A> are the index of its core-free subgroups.
#! @BeginExampleSession
#! gap> G := SymmetricGroup(4);; dh := DihedralGroup(10);;
#! gap> CoreFreeDegrees(G);
#! [ 24, 12, 8, 6, 4 ]
#! gap> CoreFreeDegrees(dh);
#! [10, 5]
#! @EndExampleSession
DeclareGlobalFunction( "CoreFreeDegrees" );

#! @Chapter Faithful Transitive Permutation Representations
#!
#! The action of a group G on the coset space of a subgroup gives us a transitive permutation representation of the group.
#! Whenever the subgroup is core-free, we have that the action of G on the coset space of the subgroup will be faithful. 
#! Moreover, the stabilizer of a point on a faithful transitive permutation representation of G will always be a core-free subgroup. 
#!
#! @Section Obtaining Faithful Transitive Permutation Representations
#!
#! @Arguments G [, all_ftpr]
#! @Returns a list
#! @Description
#!  For a finite group <A>G</A>, <A>FaithfulTransitivePermutationRepresentations</A> returns a list of a faithful transitive permutation representation of <A>G</A> for each degree.
#!  If <A>all_ftpr</A> is true, then it will return a list of all faithful transitive permutation representations.
#! @BeginExampleSession
#! gap> sp := SymplecticGroup(4,2);;
#! gap> CoreFreeDegrees(sp);
#! [ 720, 360, 240, 180, 144, 120, 90, 80, 72, 60, 45, 40, 36, 30, 20, 15, 12, 10, 6 ]
#! gap> ftprs := FaithfulTransitivePermutationRepresentations(sp);; Size(ftprs);
#! 19
#! gap> all_ftprs := FaithfulTransitivePermutationRepresentations(sp,true);; Size(all_ftprs);
#! 54
#! @EndExampleSession
DeclareOperation( "FaithfulTransitivePermutationRepresentations", [IsGroup]);


#! @Section Faithful Transitive Permutation Representation of Minimal Degree
#! 
#! To complement the already existing functions in GAP <A>MinimalFaithfulPermutationRepresentation</A> and <A>MinimalFaithfulPermutationDegree</A>,
#! the following functions to retreive the <A>MinimalFaithfulTransitivePermutationRepresentation</A> and <A>MinimalFaithfulTransitivePermutationDegree</A>.
#!
#! @Arguments G [,all_minimal_ftpr]
#! @Returns an isomorphism (or a list of isomorphisms)
#! @Description
#! For a finite group <A>G</A>, <A>MinimalFaithfulTransitivePermutationRepresentation</A> returns an isomorphism of <A>G</A> into the symmetric group of minimal degree acting transitively on its domain. 
#! If <A>all_minimal_ftpr</A> is set as <A>true</A>, then it returns a list of all isomorphisms <A>G</A> into the symmetric group of minimal degree.
#! @BeginExampleSession
#! gap> sp := SymplecticGroup(4,2);;
#! gap> min_ftpr := MinimalFaithfulTransitivePermutationRepresentation(sp);
#! CompositionMapping( <action epimorphism>, <action isomorphism> )
#! gap> min_ftpr(sp);
#! Group([ (1,6,4,3), (1,3)(2,4,6,5) ])
#! gap> min_ftprs := MinimalFaithfulTransitivePermutationRepresentation(sp,true);
#! [ CompositionMapping( <action epimorphism>, <action isomorphism> ), CompositionMapping( <action epimorphism>, <action isomorphism> ) ]
#! gap> min_ftprs[2](sp);
#! Group([ (2,3,6,5), (1,3)(2,5,6,4) ])
#! @EndExampleSession
DeclareOperation( "MinimalFaithfulTransitivePermutationRepresentation", [IsGroup]);

#! @Arguments G
#! @Returns an integer
#! @Description
#! For a finite group <A>G</A>, <A>MinimalFaithfulTransitivePermutationDegree</A> returns the least positive integer n such that <A>G</A> is isomorphic to a subgroup of the symmetric group of degree n acting transitively on its domain.
#! @BeginExampleSession
#! gap> sp := SymplecticGroup(4,2);; g:=SimpleGroup("PSL",3,5);;
#! gap> MinimalFaithfulTransitivePermutationDegree(sp);
#! 6
#! gap> MinimalFaithfulTransitivePermutationDegree(g);
#! 31
#! @EndExampleSession
DeclareGlobalFunction( "MinimalFaithfulTransitivePermutationDegree" );



