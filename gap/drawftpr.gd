##########################################################################
##
#F Splash
##
## The aim of this function is to "splash" an image directly from the dot code.
## To this effect, it adds a preamble and makes a call to the Viz Splash function.
## To avoid forcing the user to install the Viz package (under development), a copy 
## of the Viz Splash function is included in the file "CF_splashfromviz.g" of this package
###########################################################################
DeclareInfoClass( "InfoDrawFTPR" );
SetInfoLevel( InfoDrawFTPR, 1 );
#! @Chapter Drawing the Faithful Transitive Permutation Representation Graph
#! @Section Drawing functions
#!
#! One of the advantages of Faithful Transitive Permutation Representation Graph are on Groups generated by involutions, such as C-groups.
#! These graphs are very useful in the research of abstract polytopes and hypertopes, mainly called as "Schreier coset graphs" or "CPR graphs" in this area. 
#! Here we will give a function that builds this graph given a permutation group generated by involutions, a group and one of its core-free subgroups or by giving an isomorphism of the group into the symmetric group acting faithfully and transitively on its domain.
#! To use Graphviz in order to create the image file, you need to be running GAP on a Linux Environment (Windows Subsystem for Linux is supported), with graphviz installed.
#!
#! @BeginGroup
#! @Returns a graph written in dot
#! @Description
#! Given a transitive permutation group <A>G</A>, a faithful transitive permutation representation of a group <A>map</A> or a group <A>H</A> and one of its core-free subgroups <A>K</A>, the function will output the permutation representation graph written in the language of a Dot file.
#! If given a list of the name of the generators <A>generators_name</A>, these will be given to the label of their action on the graph. Otherwise, the labels will be <A>r0, r1,  r2, ...</A> for the generators <A>G.1, G.2, G.3, ...</A>.
#!
#! @BeginExampleSession
#! gap> G:= Group((1,2),(2,3),(3,4));; H :=Subgroup(G,[(1,2),(2,3)]);;
#! gap> dotprint := DotFTPRGraph(G);
#! "digraph {\n1 -> 2 [label = r1,dir=none];\n 2 -> 3 [label = r2,dir=none];\n 3 \
#! -> 4 [label = r3,dir=none];\n }\n"
#! gap> Print(dotprint);
#! digraph {
#! 1 -> 2 [label = r1,dir=none];
#!  2 -> 3 [label = r2,dir=none];
#!  3 -> 4 [label = r3,dir=none];
#!  }
#! gap> Print(DotFTPRGraph(G,H,["A","B","C"]));
#! digraph {
#! 3 -> 4 [label = A,dir=none];
#!  2 -> 3 [label = B,dir=none];
#!  1 -> 2 [label = C,dir=none];
#!  }
#! @EndExampleSession
#! @Arguments G
DeclareOperation("DotFTPRGraph", [IsPermGroup]);
#! @Arguments G[, generators_name]
DeclareOperation("DotFTPRGraph", [IsPermGroup, IsList]);
#! @Arguments map
DeclareOperation("DotFTPRGraph", [IsGeneralMapping]);
#! @Arguments map[, generators_name]
DeclareOperation("DotFTPRGraph", [IsGeneralMapping, IsList]);
#! @Arguments H, K
DeclareOperation("DotFTPRGraph", [IsGroup,IsGroup]);
#! @Arguments H, K[, generators_name]
DeclareOperation("DotFTPRGraph", [IsGroup,IsGroup, IsList]);
#! @EndGroup


#! @Returns an image of the faithful transitive permutation representation graph
#! @Description
#! This global function takes as input the following arguments:
#! * <A>arg</A> := <A>dotstring</A>[, <A>rec</A>]
#! * <A>arg</A> := <A>G</A>[, <A>rec</A>]
#! * <A>arg</A> := <A>map</A>[,<A>rec</A>]
#! * <A>arg</A> := <A>H</A>,<A>K</A>[,<A>rec</A>]
#! Given a string of a graph in dot <A>dotstring</A>, this function will output and show an image of the graph.
#! Alternatively, a transitive permutation group <A>G</A>, a faithful transitive permutation representation of a group <A>map</A> or a group <A>H</A> and one of its core-free subgroups <A>K</A>, can be given. This will use <A>DotFTPRGraph</A> to calculate the <A>dotstring</A>.
#! Moreover, extra parameters can be given as a form of a record <A>rec</A>. The set of parameters that can be given inside a record can be found below, with information regarding their effect:
#! * <A>layout</A> - (a string) the engine that is used to calculate the layout of the vertices and edges of graph to output in the dot image (not used for TeX output). The supported layouts are "dot", "neato", "twopi", "circo", "fdp", "sfdp", "patchwork", "osage". By default "neato" is used.
#! * <A>directory</A> - (a string) the name of the folder where the dot and image files are created. By default, a temporary folder of GAP is used.
#! * <A>path</A> - (a string) the path where the directory will be created. If the directory is not specified, a folder "tmp.viz" will be created at the determined path. If no path is given, the default path is "~/". If no path nor directory is given, it will be saved in a temporary path of GAP.
#! * <A>file</A> - (a string) the name of the dot and image files created. By default, the name will be "vizpicture".
#! * <A>filetype</A> - (a string) the image file type that will be created. By default, the filetype will be "pdf".
#! * <A>viewer</A> - (a string) the name of the visualizer used to open the image. The supported ones are "evince","xpdf","xdg-open","okular", "gv", "open" (for the different System Architectures).
#! * <A>tikz</A> - (a boolean) if true, then the function will produce a TeX file, compile it to pdf and open.
#! * <A>viewtexfile</A> - (a boolean) if true, then the function will produce a TeX file and return the text of the Tex file (but it will not compile and open any pdf from the TeX file).
#!
#! @BeginExampleSession
#! gap> G:= SymmetricGroup(4);;H:= Subgroup(G,[(1,2)]);;K:= Subgroup(G,[(1,2,3)]);;
#! gap> DrawFTPRGraph(G);
#! gap> texfile := DrawFTPRGraph(G,H,rec(viewtexfile := true));; 
#! gap> Print(texfile{[1..115]});
#! \documentclass{article}
#! \usepackage[x11names, svgnames, rgb]{xcolor}
#! \usepackage[utf8]{inputenc}
#! \usepackage{tikz}
#! gap> DrawFTPRGraph(FactorCosetAction(G,K),rec(directory := "myfolder", layout:="fdp"));;
#! @EndExampleSession
DeclareGlobalFunction("DrawFTPRGraph");

#! @Returns an image of the faithful transitive permutation representation graph
#! @Description
#! The same as <A>DrawFTPRGraph</A> with the parameter <A>viewtexfile := true</A>.
DeclareGlobalFunction("TeXFTPRGraph");

#! @Returns an image of the faithful transitive permutation representation graph
#! @Description
#! The same as <A>DrawFTPRGraph</A> with the parameter <A>tikz := true</A>.
DeclareGlobalFunction("DrawTeXFTPRGraph");
#!
#! @Section Information Level of Drawing Functions
#! We can set the amount of verbosity of the functions "DrawFTPRGraph", "TeXFTPRGraph" and "DrawTeXFTPRGraph", which can be controlled 
#! by the <A>InfoDrawFTPR</A> variable.
#! As of right now, there are only two levels of the <A>InfoDrawFTPR</A> and, by default, the level is set as 1.
#! To change to level 2, you can do the following:
#! @BeginLogSession
#! gap> SetInfoLevel(InfoDrawFTPR,2);
#! @EndLogSession
#! Particularly, <A>InfoDrawFTPR</A> in level 2 will give information regarding the location in which the files are being created and processed.

