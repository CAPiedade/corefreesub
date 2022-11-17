#
# corefreesub: A GAP Package for calculating the core-free subgroups and their faithful transitive permutation representations
#
# Implementations
#
InstallGlobalFunction( IsCoreFree,
function(G,H)
	if Size(Core(G,H)) = 1 then return true; else return false; fi; 
end);

InstallGlobalFunction( CoreFreeConjugacyClassesSubgroups,
function(G)
	local iso, H, n, i;
	if not IsPermGroup(G) then 
		iso := IsomorphismPermGroup(G);
		H := Image(iso);
	else
		H := G;
		iso := IdentityMapping(G);
	fi;
	return List(Filtered(ConjugacyClassesSubgroups(H), n -> IsCoreFree(H,n[1])), i -> ConjugacyClassSubgroups(G,PreImage(iso,Representative(i))) );
end );

InstallGlobalFunction( AllCoreFreeSubgroups,
function(G)
	local i, j;
	return Flat(List(CoreFreeConjugacyClassesSubgroups(G), i -> List(i, j -> j)));
end );

InstallGlobalFunction( CoreFreeDegrees,
function(G)
	local n;
	return Unique(List(CoreFreeConjugacyClassesSubgroups(G), n -> Index(G,n[1])));
end );


InstallMethod( MinimalFaithfulTransitivePermutationRepresentation, [IsGroup], 
function(G)
	return FactorCosetAction(G,Reversed(CoreFreeConjugacyClassesSubgroups(G))[1][1]);
end );

InstallOtherMethod( MinimalFaithfulTransitivePermutationRepresentation, [IsGroup, IsBool], 
function(G,bool) #bool here is either "we want all minimal degree FTPRs" - true , or "only one of the minimal degree FTPR"
	local core_free_ccs;
	if bool then
		core_free_ccs := Reversed(CoreFreeConjugacyClassesSubgroups(G));
		return List(Filtered(core_free_ccs, i -> Index(G,i[1]) = Index(G,core_free_ccs[1][1])), j -> FactorCosetAction(G,j[1]));
	else
		return MinimalFaithfulTransitivePermutationRepresentation(G);
	fi;
end );


InstallGlobalFunction( MinimalFaithfulTransitivePermutationRepresentationDegree,
function(G)
	local n;
	return Minimum(CoreFreeDegrees(G));
end );

InstallMethod( FaithfulTransitivePermutationRepresentations, [IsGroup], 
function(G)
	return List(CoreFreeDegrees(G), i -> FactorCosetAction(G,First(CoreFreeConjugacyClassesSubgroups(G), j -> Index(G,j[1]) = i)[1]));
end );

InstallOtherMethod( FaithfulTransitivePermutationRepresentations, [IsGroup, IsBool], 
function(G,bool) #bool here is either "we want all FTPRs" - true, or "only one for each degree"
	if bool then
		return List(CoreFreeConjugacyClassesSubgroups(G), i -> FactorCosetAction(G,i[1]));
	else
		return FaithfulTransitivePermutationRepresentations(G);
	fi;
end );



InstallMethod( DrawFTPRGraph, [IsPermGroup,IsString, IsString, IsString],
function( G , filename, layout, filetype )
	local filename, vertices, edges, graph;
	if layout not in ["dot", "neato", "twopi", "circo", "fdp", "sfdp", "patchwork", "osage"] then
		return Error(Concatenation("Usage DrawFTPRGraph: layout should be one of the supported by Graphviz, not ",layout,".\n"));
	if filetype not in ["dot", "xdot", "ps", "pdf", "svg", "svgz", "png", "gif", "jpg", "jpeg", "json", "imap", "cmapx"] then
		return Error(Concatenation("Usage DrawFTPRGraph: filetype ",filetype," is not supported. Please choose one supported by Graphviz.\n"));
	edge_dic := [];
	gen_index := 0;
	for gen in GeneratorsOfGroup(G) do 
		already_moved := [];
		Append(edge_dic, [[Concatenation("r",String(gen_index)),[]]]);
		for num in MovedPoints(gen) do
			if num in already_moved then continue;
			else 
				Add(edge_dic[gen_index+1][2],[num,OnPoints(num,gen)]);
				Append(already_moved,[num,OnPoints(num,gen)]);
			fi;
		od;
		gen_index := gen_index + 1;
	od;

    dot_string := "graph {\n";

    for gen_edge in edge_dic do
		for edge in gen_edge[2] do 
			dot_string:=Concatenation(dot_string, String(edge[1]) , " -- ", String(edge[2]), " [label = ", gen_edge[1],"];\n ");
		od;
	od;
    dot_string:=Concatenation(dot_string, "}\n");    
    PrintTo( Concatenation(filename, ".dot") , dot_string );
    IsGraphvizInstalled :=  function(layout)
		local str, outputtext, path, which_prg ;
		if not ARCH_IS_UNIX() then
		Info(InfoCoreFreeSub,1,"Package 'CoreFreeSub': Use an Unix architecture to produce the Graph using Graphviz.");
		return false;
		else
			str:=""; outputtext:=OutputTextString(str, true);
			path := DirectoriesSystemPrograms();;
			which_prg := Filename( path, "which" );
			Process(DirectoryCurrent(), which_prg,  InputTextNone(), outputtext,[layout]);
			if str = "" then
				Info(InfoCoreFreeSub,1,Concatenation("Package 'CoreFreeSub': The layout ", layout ," of Graphviz is not installed on your system. Please install Graphviz."));
				return false;
			fi;
		fi;
		return true;
	end;
    if IsGraphvizInstalled() then
        Exec( Concatenation(layout," -T",filetype, " ", filename, ".dot -o ", filename, ".",filetype) );
		Info(InfoCoreFreeSub,1, "File written to current path");
    else
        Info(InfoCoreFreeSub, 1, "Only .dot file was written to current path. Graphviz or Layout choosen is not available");
    fi;	
    return;
  end );



InstallMethod( DrawFTPRGraph, [IsGeneralMapping, IsString, IsString, IsString],
function( FTPR_mapping, filename, layout, filetype )
	return DrawFTPRGraph(Image(FTPR_mapping), filename, layout, filetype);
end);

InstallMethod( DrawFTPRGraph, [IsGroup, IsGroup, IsString, IsString, IsString],
function( G, H, filename, layout, filetype)
	if IsSubgroup(G,H) then
		return DrawFTPRGraph(Image(FactorCosetAction(G,H)), filename, layout, filetype);
	else
		Info(InfoCoreFreeSub,1,"The second group should be a subgroup of the first");
		return fail;
end);


InstallOtherMethod( DrawFTPRGraph, [IsPermGroup, IsString, IsString],
function(G, filename, layout_or_filetype)
	if layout_or_filetype in ["dot", "xdot", "ps", "pdf", "svg", "svgz", "png", "gif", "jpg", "jpeg", "json", "imap", "cmapx"] 
		and layout_or_filetype not in ["dot", "neato", "twopi", "circo", "fdp", "sfdp", "patchwork", "osage"] then
		return DrawFTPRGraph(G, filename, "neato", layout_or_filetype);
	elif layout_or_filetype in ["dot", "neato", "twopi", "circo", "fdp", "sfdp", "patchwork", "osage"]
		and layout_or_filetype not in ["dot", "xdot", "ps", "pdf", "svg", "svgz", "png", "gif", "jpg", "jpeg", "json", "imap", "cmapx"] then
		return DrawFTPRGraph(G, filename, layout_or_filetype, "svg");
	elif layout_or_filetype = "dot" then DrawFTPRGraph(G, filename, "dot", "dot");
	else 
		Info(InfoCoreFreeSub,1,"Please give a valid layout or filetype");
		return fail;
	fi;
end);

InstallOtherMethod( DrawFTPRGraph, [IsGroup, IsGroup, IsString, IsString],
function(G, H, filename, layout_or_filetype)
	return DrawFTPRGraph(Image(FactorCosetAction(G,H)), filename, layout_or_filetype);
end);

InstallOtherMethod( DrawFTPRGraph, [IsPermGroup, IsString],
function(G, filename)
	return DrawFTPRGraph(G, filename, "neato", ".svg");
end);

InstallOtherMethod( DrawFTPRGraph, [IsGeneralMapping, IsString],
function(FTPR_mapping , filename)
	return DrawFTPRGraph(Image(FTPR_mapping), filename, "neato", ".svg");
end);

InstallOtherMethod( DrawFTPRGraph, [IsGroup, IsGroup, IsString],
function(G, H, filename)
	return DrawFTPRGraph(Image(FactorCosetAction(G,H)), filename, "neato", ".svg");
end);



