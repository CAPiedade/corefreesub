########################################################################
##
##  CoreFreeSub package
##
##  Copyright 2022
##    A GAP Package for calculating the core-free subgroups and their faithful transitive permutation representations
##    
##    
##
## Licensed under the GPL 2 or later.
##
########################################################################
# 
DeclareInfoClass( "InfoCoreFreeSub" );
SetInfoLevel( InfoCoreFreeSub, 1 );
#! @Chapter Introduction
#!
#! The &corefreesub; package was created to calculate core-free subgroups of a group, their indexes, and faithful transitive permutation representations.
#! 
#! A core-free subgroup of a group <A>G</A> is a subgroup <A>H</A> such that
#! $$ \cap_{g\in G} H =  \{id_G\}. $$
#! These subgroups are important since the action of <A>G</A> on the cosets of <A>H</A> is both transitive and faithful.
#! Hence, this gives us a faithful transitive permutation representation of <A>G</A> with degree <A>n</A>, where <A>n</A> is the index of <A>H</A> in <A>G</A>.
#!
#! There are many articles studying faithful permutation representation of groups, such as <Cite Key="johnson_minimal_1971"/>, <Cite Key="easdown_minimal_1988"/>, <Cite Key="saunders_minimal_2014"/> and <Cite Key="easdown_minimal_2016"/>.
#! However the restriction on transitive actions is more recent and there are fewer studies like <Cite Key="FP20Tor"/>,<Cite Key="FP21Cor"/>,<Cite Key="FP21Hyper"/> and <Cite Key="FP22Loc"/>.
#!
#! During C.A. Piedade's PhD thesis, he studied many of these faithful transitive permutation representations of automorphism groups of abstract regular polytopes and hypertopes.
#! It was also during this period that this author noticed the abcense of functions/methods in GAP to compute core-free subgroups of a group.
#! As a consequence, he created many functions to help in his research, resulting in many of the functions and methods implemented in this package.
#! 
#! One of the important tools for studying faithful transitive permutation representations is by using <A>faithful transitive permutation representation graphs</A>, which are <A>Schreier coset graphs</A>.
#! A <A>Schreier coset graph</A> is a graph associated with a group <A>G</A>, its generators and a subgroup <A>H</A> of <A>G</A>. 
#! The vertices of the graph are the right cosets of <A>H</A> and there is a directed edge $(Hx,Hy)$
#! with label $g$ if $g$ is a generator of <A>G</A> and $Hxg = Hy$. When $g$ is an involution, the two directed edges
#! $(Hx, Hy)$ and $(Hy, Hx)$ are replaced by a single undirected edge $\{Hx, Hy\}$ with label $g$.
#!
#! In the &corefreesub; package, this can achieved by creating graphs as DOT files and using an adaptation of the visualization package developed by M. Delgado et al. <Cite Key="IntPic"/> <Cite Key="Automata"/>, which can be found on Chapter 4.
#!
#! This package was created using the GAP Package <A>PackageMaker</A> <Cite Key="PackageMaker"/>, with documentation done using <A>AutoDoc</A> <Cite Key="AutoDoc"/>.
#!
#! @Section Instalation
#! 
#! To install this package, you can simply copy the folder of &corefreesub; and its contents into your <A>/pkg</A> folder inside your &GAP; instalation folder.
#! This should work for Windows, Ubuntu and MacOS.
#! If you are using GAP.app on MacOS, the &corefreesub; folder should be copied into your user Library/Preferences/GAP/pkg folder. 
#!
#! This package was tested with &GAP; version greater or equal to 4.11.
#! 
#! @Section Testing your instalation
#! 
#! To test your instalation, you can run the function <A>CF_TESTALL()</A>. 
#! This function will run two sets of tests, one dependent on the
#! documentation of the &corefreesub; package and another with assertions with groups with bigger size.
#! 
#! If the test runs with no issue, the output should look something similar to the following:
#! @BeginLogSession
#! gap> CF_TESTALL();
#! Running list 1 . . .
#! gap>
#! @EndLogSession
#! This tests will also produce two pictures that are supposed to be outputed and open in the user system.
#! If the tests run with no error but they do not output any of the graphs, then it may mean the user might not be able to
#! use this functionality. If so, please report an issue on <URL Text="CoreFreeSub GitHub Issues">https://github.com/CAPiedade/corefreesub/issues</URL>.
#! 
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
#! gap> Core(G,H);# H is a normal subgroup of G, hence it does not have a trivial core
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
#! [ Group( () )^G, Group( [ (1,3)(2,4) ] )^G, Group( [ (3,4) ] )^G,
#! Group( [ (2,4,3) ] )^G, Group( [ (3,4), (1,2)(3,4) ] )^G,
#! Group( [ (1,3,2,4), (1,2)(3,4) ] )^G, Group( [ (3,4), (2,4,3) ] )^G ]
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
#! [ Group(()), Group([ (1,3)(2,4) ]), Group([ (1,4)(2,3) ]), Group([ (1,2)(3,4) ]),
#!  Group([ (3,4) ]), Group([ (2,4) ]), Group([ (2,3) ]), Group([ (1,4) ]),
#!  Group([ (1,3) ]), Group([ (1,2) ]), Group([ (2,4,3) ]), Group([ (1,3,2) ]),
#!  Group([ (1,3,4) ]), Group([ (1,4,2) ]), Group([ (3,4), (1,2)(3,4) ]),
#!  Group([ (2,4), (1,3)(2,4) ]), Group([ (2,3), (1,4)(2,3) ]),
#!  Group([ (1,3,2,4), (1,2)(3,4) ]), Group([ (1,2,3,4), (1,3)(2,4) ]),
#!  Group([ (1,2,4,3), (1,4)(2,3) ]), Group([ (3,4), (2,4,3) ]),
#!  Group([ (1,3), (1,3,2) ]), Group([ (1,3), (1,3,4) ]), Group([ (1,4), (1,4,2) ])
#! ]
#! gap> AllCoreFreeSubgroups(dh);
#! [ Group([  ]), Group([ f1 ]), Group([ f1*f2^2 ]), Group([ f1*f2^4 ]), 
#! Group([ f1*f2 ]), Group([ f1*f2^3 ]) ]
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
#! [ 720, 360, 240, 180, 144, 120, 90, 80, 72, 60, 45, 40, 36, 30, 20, 15, 12,
#! 10, 6 ]
#! gap> ftprs := FaithfulTransitivePermutationRepresentations(sp);; 
#! gap> Size(ftprs);
#! 19
#! gap> all_ftprs := FaithfulTransitivePermutationRepresentations(sp,true);; 
#! gap> Size(all_ftprs);
#! 54
#! @EndExampleSession
DeclareOperation( "FaithfulTransitivePermutationRepresentations", [IsGroup]);


#! @Section Faithful Transitive Permutation Representation of Minimal Degree
#! 
#! To complement the already existing functions in GAP <A>MinimalFaithfulPermutationDegree</A> and <A>MinimalFaithfulPermutationRepresentation</A>,
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
#! [ CompositionMapping( <action epimorphism>, <action isomorphism> ), 
#! CompositionMapping( <action epimorphism>, <action isomorphism> ) ]
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



