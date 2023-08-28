#
# corefreesub: A GAP Package for calculating the core-free subgroups and their faithful transitive permutation representations
#
# Drawing of FTPR and Dot Files
#
# 
##########################################
# Some functions to be used to check requisites
#

InstallMethod( DotFTPRGraph,[IsPermGroup,IsList],
function(G, genList)
	local edge_dic, gen, gen_index, already_moved, num, dot_string, gen_edge, edge, isdigraph;
	if not IsTransitive(G) then 
		return Error("The group given should be transitive");
	fi;
	if Size(genList) <> Size(GeneratorsOfGroup(G)) then
		return Error("The size of the generators name list is different from the size of the generators of the group given");
	fi;

	edge_dic := [];
	gen_index := 0;
	for gen in GeneratorsOfGroup(G) do 
		already_moved := [];
		Append(edge_dic, [[genList[gen_index+1],[]]]);
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

InstallOtherMethod( DotFTPRGraph, [IsPermGroup],
function(G)
	return DotFTPRGraph(G,List([1 .. Size(GeneratorsOfGroup(G))], i -> Concatenation("r",String(i))));
end );

InstallOtherMethod( DotFTPRGraph, [IsGroup,IsGroup, IsList],
function( G, H, genList )
	if IsSubgroup(G,H) and IsCoreFree(G,H) then
		return DotFTPRGraph(Image(FactorCosetAction(G,H)), genList);
	else
		Info(InfoDrawFTPR,1,"The second group should be a subgroup of the first or it is not core-free");
		return fail;
	fi;
end);

InstallOtherMethod( DotFTPRGraph, [IsGroup,IsGroup],
function( G, H )
	return DotFTPRGraph(G, H,List([1 .. Size(GeneratorsOfGroup(G))], i -> Concatenation("r",String(i))));
end);

InstallOtherMethod( DotFTPRGraph, [IsGeneralMapping, IsList],
function(FTPR_mapping, genList)
	if IsGroupHomomorphism(FTPR_mapping) and IsBijective(FTPR_mapping) and IsPermGroup(Image(FTPR_mapping)) and IsTransitive(Image(FTPR_mapping)) then
		return DotFTPRGraph(Image(FTPR_mapping), genList);
	else
		Info(InfoDrawFTPR,1,"The mapping provided isn't of a faithful transitive permutation representation");
		return fail;
	fi;
end );

InstallOtherMethod( DotFTPRGraph, [IsGeneralMapping],
function(FTPR_mapping)
	return DotFTPRGraph(FTPR_mapping, List([1 .. Size(GeneratorsOfGroup(Image(FTPR_mapping)))], i -> Concatenation("r",String(i))));
end );





# The aim of the following functions is to "splash" an image or TeX directly from the dot code.
# To this effect, it adds a preamble and makes a call to the Viz Splash function.
# To avoid forcing the user to install the Viz package (under development), a modified copy of the Viz Splash function is included in the file "splashfromViz.g" of this package


InstallGlobalFunction( DrawFTPRGraph,
function( arg )
	local opt;
	opt := First(arg, k -> IsRecord(k));
	if opt <> fail and IsBound(opt.viewtex) and opt.viewtex = true then return Splash(arg);fi;
	Splash(arg);
end );

InstallGlobalFunction( TeXFTPRGraph,
function( arg )
	local opt, pos;
	opt := First(arg, k -> IsRecord(k));
	if opt = fail then
		opt := rec();
		opt.viewtexfile := true;
		opt.tikz := true;
		Append(arg,[opt]);
	else
		pos := Position(arg,opt);
		opt.viewtexfile := true;
		opt.tikz := true;
		arg[pos] := opt;
	fi;
	return Splash(arg);
end );

InstallGlobalFunction( DrawTeXFTPRGraph,
function( arg )
	local opt, pos;
	opt := First(arg, k -> IsRecord(k));
	if opt = fail then
      opt := rec();
	  opt.tikz := true;
	  Append(arg,[opt]);
	  else
	pos := Position(arg,opt);
	opt.tikz := true;
	arg[pos] := opt;
	fi;
	Splash(arg);
end );


