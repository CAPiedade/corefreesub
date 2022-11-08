#
# corefreesub: A GAP Package for calculating the core-free subgroups and their faithful transitive permutation representations
#
# Implementations
#
InstallGlobalFunction( CoreFreeSubs,
function(G)
	local
	if not IsPermGroup(G) then G := Image(IsomorphismPermGroup(G)) fi;
	Print( "This is a placeholder function, replace it with your own code.\n" );
end );

InstallGlobalFunction( CoreFreeDegrees,
function(G)
	local 
	subgroups = CoreFreeSubs(G)

end );
