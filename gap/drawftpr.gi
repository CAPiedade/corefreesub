#
# corefreesub: A GAP Package for calculating the core-free subgroups and their faithful transitive permutation representations
#
# Drawing of FTPR and Dot Files
#
# 
##########################################
# Some functions to be used to check requisites
#
IsItInstalled :=  function(IT)
	local str, outputtext, path, which_prg ;
	if not ARCH_IS_UNIX() then
	Info(InfoCoreFreeSub,1,"Package 'CoreFreeSub': Use an Unix architecture to produce the Graph using Graphviz.");
	return false;
	else
		str:=""; outputtext:=OutputTextString(str, true);
		path := DirectoriesSystemPrograms();;
		which_prg := Filename( path, "which" );
		Process(DirectoryCurrent(), which_prg,  InputTextNone(), outputtext,[IT]);
		if str = "" then
			return false;
		fi;
	fi;
	return true;
end;

AvailableProgramsFTPRGraph := function()
	local programs, display_programs, pdf_viewer, graphviz_layouts_supported, graphviz_installed;
	programs := Filtered(["dot2tex"], n -> IsItInstalled(n));
	if Size(programs) = 0 then
		Error("Dot2Tex not installed");
	fi;
	display_programs := Filtered(["display", "feh", "fim", "viu", "xdg-open"], n -> IsItInstalled(n));
	if display_programs = ["xdg-open"] then
		Info(InfoCoreFreeSub,1,"None of the supported display programs are installed. The user's preferred application for this file will be used.");
	elif Size(display_programs) = 0 then
		Error("None of the supported display programs are installed. Please install one of the following: display, feh, fim, viu");
	fi;
	pdf_viewer := Filtered(["okular", "evince", "xdg-open"], n -> IsItInstalled(n));
	if pdf_viewer = ["xdg-open"] then
		Info(InfoCoreFreeSub,1,"None of the supported pdf viewer programs are installed. The user's preferred application for this file will be used.");
	elif Size(pdf_viewer) = 0 then
		Error("None of the supported display programs are installed. Please install one of the following: okular, evince.");
	fi;
	graphviz_layouts_supported := ["dot", "neato", "twopi", "circo", "fdp", "sfdp", "patchwork", "osage"];
	graphviz_installed := Filtered(graphviz_layouts_supported , n -> IsItInstalled(n));
	if Size(graphviz_installed) = 0 then
		Error("None of the supported GraphViz layouts are installed. Please install GraphViz.");
	fi;
	return	[programs[1],display_programs[1],pdf_viewer[1],graphviz_layouts_supported];
end;
##########################################
# Functions that can be called in GAP


InstallMethod( DotFTPRGraph, [IsPermGroup],
function(G)
	local edge_dic, gen, gen_index, already_moved, num, dot_string, gen_edge, edge, isdigraph;
	isdigraph := false;
	if not IsTransitive(G) then 
		return Error("The group given should be transitive");
	fi;

	edge_dic := [];
	gen_index := 0;
	for gen in GeneratorsOfGroup(G) do 
		already_moved := [];
		Append(edge_dic, [[Concatenation("r",String(gen_index)),[]]]);
		for num in MovedPoints(gen) do
			if num in already_moved then continue;
			elif OnPoints(OnPoints(num,gen),gen) = num then 
				Add(edge_dic[gen_index+1][2],[num,OnPoints(num,gen),false]);
				Append(already_moved,[num,OnPoints(num,gen)]);
			else
				Add(edge_dic[gen_index+1][2],[num,OnPoints(num,gen),true]);
				Append(already_moved,[num]);
			fi;
		od;
		gen_index := gen_index + 1;
	od;
	dot_string := "digraph {\n";

    for gen_edge in edge_dic do
		for edge in gen_edge[2] do 
		if edge[3] then
			dot_string:=Concatenation(dot_string, String(edge[1]) , " -> ", String(edge[2]), " [label = ", gen_edge[1],"];\n ");
		else 
			dot_string:=Concatenation(dot_string, String(edge[1]) , " -> ", String(edge[2]), " [label = ", gen_edge[1],",dir=none];\n ");
		fi;
		od;
	od;
    dot_string:=Concatenation(dot_string, "}\n");
	return dot_string;
end );

InstallOtherMethod( DotFTPRGraph, [IsGroup,IsGroup],
function( G, H )
	if IsSubgroup(G,H) and IsCoreFree(G,H) then
		return DotFTPRGraph(Image(FactorCosetAction(G,H)));
	else
		Info(InfoDrawFTPR,1,"The second group should be a subgroup of the first or it is not core-free");
		return fail;
	fi;
end);

InstallOtherMethod( DotFTPRGraph, [IsGeneralMapping],
function(FTPR_mapping)
	if IsGroupHomomorphism(FTPR_mapping) and IsBijective(FTPR_mapping) and IsPermGroup(Image(FTPR_mapping)) and IsTransitive(Image(FTPR_mapping)) then
		return DotFTPRGraph(Image(FTPR_mapping));
	else
		Info(InfoDrawFTPR,1,"The mapping provided isn't of a faithful transitive permutation representation");
		return fail;
	fi;
end );


InstallMethod( DrawImageFTPRGraph, [IsPermGroup,IsString, IsString],
function( G , layout, filetype )
	local temp_location, path, image_file_path, tmp_path, available ;
	available := AvailableProgramsFTPRGraph();
	if not layout in available[3] then
		return Error(Concatenation("Usage DrawFTPRGraph: layout '",layout,"' might not be installed or it is not one of the supported by Graphviz.\n"));
	fi;
	if not filetype in ["dot", "xdot", "ps", "pdf", "svg", "svgz", "png", "gif", "jpg", "jpeg", "json", "imap", "cmapx"] then
		return Error(Concatenation("Usage DrawFTPRGraph: filetype '",filetype,"' is not supported. Please choose one supported by Graphviz.\n"));
	fi;
    temp_location := DirectoryTemporary();
    path := Filename(temp_location,"graph.dot");
    tmp_path := Filename(temp_location,"");
    image_file_path := Filename(temp_location,Concatenation("graph.",filetype));
    PrintTo(path, DotFTPRGraph(G));
    if IsItInstalled(layout) then
		if "display" = available[2] then
			Exec( Concatenation(layout," -T",filetype, " ",path," -o ",image_file_path ));
			Info(InfoCoreFreeSub,1, Concatenation("File written temporarily to path ", image_file_path));
			Exec( Concatenation("display ", image_file_path));
		else
			Exec( Concatenation(layout," -T",filetype, " ",path," -o ",image_file_path ));
			Info(InfoCoreFreeSub,1, Concatenation("Display not installed. File written temporarily to path ", image_file_path));
		fi;
	else
        Info(InfoCoreFreeSub, 1, "Graphviz might not be available or chosen layout is not installed");
    fi;	
    return true;
end );




InstallMethod( TeXFTPRGraph, [IsPermGroup, IsString],
function(G, layout)
	local temp_location, path, tex_file_path, tmp_path, t;
	if not layout in ["dot", "neato", "twopi", "circo", "fdp", "sfdp", "patchwork", "osage"] then
		return Error(Concatenation("Usage TeXFTPRGraph: layout should be one of the supported by Graphviz, not ",layout,".\n"));
	fi;
	if not IsItInstalled(layout) then
		return Error(Concatenation("Usage TeXFTPRGraph: GraphViz layout ",layout ," not installed."));
	fi;
	if not IsItInstalled("dot2tex") then
		return Error("Usage TeXFTPRGraph: Dot2TeX not installed.");
	fi;
	temp_location := DirectoryTemporary();
    path := Filename(temp_location,"graph.dot");
    tmp_path := Filename(temp_location,"");
    tex_file_path := Filename(temp_location,"graph.tex");
    PrintTo(path, DotFTPRGraph(G));
	Exec(Concatenation(layout," -Txdot ", path," | dot2tex -ftikz -s > ",tex_file_path));
	return Concatenation("TeX file written to folder ",tex_file_path);
end );

InstallOtherMethod( TeXFTPRGraph, [IsGroup,IsGroup, IsString],
function( G, H, layout )
	if IsSubgroup(G,H) and IsCoreFree(G,H) then
		return TeXFTPRGraph(Image(FactorCosetAction(G,H)), layout);
	else
		Info(InfoDrawFTPR,1,"The second group should be a subgroup of the first or it is not core-free");
		return fail;
	fi;
end);

InstallOtherMethod( TeXFTPRGraph, [IsGeneralMapping, IsString],
function(FTPR_mapping, layout)
	if IsGroupHomomorphism(FTPR_mapping) and IsBijective(FTPR_mapping) and IsPermGroup(Image(FTPR_mapping)) and IsTransitive(Image(FTPR_mapping)) then
		return TeXFTPRGraph(Image(FTPR_mapping), layout);
	else
		Info(InfoDrawFTPR,1,"The mapping provided isn't of a faithful transitive permutation representation");
		return fail;
	fi;
end );

InstallOtherMethod( TeXFTPRGraph, [IsPermGroup],
function(G)
	Info(InfoDrawFTPR,1,"No layout provided. Using 'neato' as default");
	return TeXFTPRGraph(G,"neato");
end );

InstallOtherMethod( TeXFTPRGraph, [IsGroup,IsGroup],
function(G,H)
	Info(InfoDrawFTPR,1,"No layout provided. Using 'neato' as default");
	return TeXFTPRGraph(G,H,"neato");
end );

InstallOtherMethod( TeXFTPRGraph, [IsGeneralMapping],
function(FTPR_mapping)
	Info(InfoDrawFTPR,1,"No layout provided. Using 'neato' as default");
	return TeXFTPRGraph(Image(FTPR_mapping), "neato");
end );


InstallMethod( DrawTeXFTPRGraph, [IsPermGroup,IsString],
function( G , layout )
	local output, path;
	output := TeXFTPRGraph(G,layout);
	Print(output,"\n");
	path := List([28..Size(output)], n -> output[n]);
	Error("IN DEVELOPMENT!!!");
end );


