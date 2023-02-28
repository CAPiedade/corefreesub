#
# corefreesub: A GAP Package for calculating the core-free subgroups and their faithful transitive permutation representations
#
# Drawing of FTPR and Dot Files
#
# 


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
		return DotFTPRGraph(Image(FactorCosetAction(G,H)), filename, layout, filetype);
	else
		Info(InfoDrawFTPR,1,"The second group should be a subgroup of the first or it is not core-free");
		return fail;
	fi;
end);

InstallOtherMethod( DotFTPRGraph, [IsGeneralMapping],
function(FTPR_mapping)
	if IsGroupHomomorphism(FTPR_mapping) and IsBijective(FTPR_mapping) and IsPermGroup(Image(FTPR_mapping)) and IsTransitive(Image(FTPR_mapping)) then
		return DotFTPRGraph(Image(FTPR_mapping), filename, layout, filetype);
	else
		Info(InfoDrawFTPR,1,"The mapping provided isn't of a faithful transitive permutation representation");
		return fail;
	fi;
end );


InstallMethod( DrawImageFTPRGraph, [IsPermGroup,IsString, IsString],
function( G , layout, filetype )
	local IsGraphvizInstalled, temp_location, path, image_file_path, tmp_path ;
	if not layout in ["dot", "neato", "twopi", "circo", "fdp", "sfdp", "patchwork", "osage"] then
		return Error(Concatenation("Usage DrawFTPRGraph: layout should be one of the supported by Graphviz, not ",layout,".\n"));
	fi;
	if not filetype in ["dot", "xdot", "ps", "pdf", "svg", "svgz", "png", "gif", "jpg", "jpeg", "json", "imap", "cmapx"] then
		return Error(Concatenation("Usage DrawFTPRGraph: filetype ",filetype," is not supported. Please choose one supported by Graphviz.\n"));
	fi;
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
    temp_location := DirectoryTemporary();
    path := Filename(temp_location,"graph.dot");
    tmp_path := Filename(temp_location,"");
    image_file_path := Filename(temp_location,Concatenation("graph.",filetype));
    PrintTo(path, DotFTPRGraph(G));
    if IsGraphvizInstalled(layout) then
        Exec( Concatenation(layout," -T",filetype, " ",path," -o ",image_file_path ));
        Info(InfoCoreFreeSub,1, Concatenation("File written temporarily to path ", image_file_path));
		Exec( Concatenation("display ", image_file_path));
    else
        Info(InfoCoreFreeSub, 1, "Graphviz might not be available or chosen layout is not installed");
    fi;	
    Exec( Concatenation("rm ", tmp_path , "graph.*"));
    return true;
end );




IsDisplayInstalled :=  function()
    local str, outputtext, path, which_prg ;
    if not ARCH_IS_UNIX() then
    Info(InfoCoreFreeSub,1,"Package 'CoreFreeSub': Use an Unix architecture to produce the Graph using Graphviz.");
    return false;
    else
        str:=""; outputtext:=OutputTextString(str, true);
        path := DirectoriesSystemPrograms();;
        which_prg := Filename( path, "which" );
        Process(DirectoryCurrent(), which_prg,  InputTextNone(), outputtext,["display"]);
        if str = "" then
            Info(InfoCoreFreeSub,1,Concatenation("Package 'CoreFreeSub': The display of Graphviz is not installed on your system. Please install Graphviz."));
            return false;
        fi;
    fi;
    return true;
end;