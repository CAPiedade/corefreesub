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
#!  Returns whether the subgroup <A>H</A> is core-free in its parent group <A>G</A>
#! @BeginExampleSession
#! gap> LoadPackage("CoreFreeSub");
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
#! gap> LoadPackage("CoreFreeSub");
#! gap> G := SymmetricGroup(4);; dh := DihedralGroup(10);;
#! gap> CoreFreeConjugacyClassesSubgroups(G);
#! [ Group( () )^G, Group( [ (1,3)(2,4) ] )^G, Group( [ (3,4) ] )^G, Group( [ (2,4,3) ] )^G, Group( [ (1,4)(2,3), (1,3)(2,4) ] )^G, Group( [ (3,4), (1,2)(3,4) ] )^G, Group( [ (1,3,2,4), (1,2)(3,4) ] )^G, Group( [ (3,4), (2,4,3) ] )^G, Group( [ (1,4)(2,3), (1,3)(2,4), (3,4) ] )^G, Group( [ (1,4)(2,3), (1,3)(2,4), (2,4,3) ] )^G, Group( [ (1,4)(2,3), (1,3)(2,4), (2,4,3), (3,4) ] )^G ]
#! gap> CoreFreeConjugacyClassesSubgroups(dh);
#! [ Group( <identity> of ... )^G, Group( [ f1 ] )^G ] 
#!@EndExampleSession
DeclareGlobalFunction( "CoreFreeConjugacyClassesSubgroups" );

#!
#! 
#! @Arguments G
#! @Returns a list
#! @Description
#!  Returns a list of all core-free subgroups of <A>G</A>
#! @BeginExampleSession
#! gap> LoadPackage("CoreFreeSub");
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
#!
#! @Arguments G
#! @Returns a list
#! @Description
#!  Returns a list of all possible degrees of faithful transitive permutation representations of <A>G</A>. The degrees of a faithful transitive permutation representation of <A>G</A> are the index of its core-free subgroups.
#! @BeginExampleSession
#! gap> LoadPackage("CoreFreeSub");
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
#! @Arguments G, [all_ftpr]
#! @Returns a list
#! @Description
#!  Returns a list of a faithful transitive permutation representation of <A>G</A> for each degree.
#!  If <A>all_ftpr</A> is true, then it will return a list of all faithful transitive permutation representations.
#! @BeginExampleSession
#! gap> LoadPackage("CoreFreeSub");
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

DeclareOperation( "MinimalFaithfulTransitivePermutationRepresentation", [IsGroup]);


DeclareGlobalFunction( "MinimalFaithfulTransitivePermutationRepresentationDegree" );

DeclareOperation("DrawFTPRGraph", [IsPermGroup,IsString,IsString,IsString];)

DeclareInfoClass( "InfoCoreFreeSub" );
SetInfoLevel( InfoCoreFreeSub, 1 );

