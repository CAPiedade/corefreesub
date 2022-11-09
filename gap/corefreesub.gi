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
	fi;
	return List(Filtered(ConjugacyClassesSubgroups(H), n -> IsCoreFree(H,n[1])), i -> ConjugacyClassSubgroups(G,PreImage(iso,Representative(i))) );
end );

InstallGlobalFunction( AllCoreFreeSubgroups,
function(G)
	return Flat(List(CoreFreeConjugacyClassesSubgroups(G), i -> List(i, j -> j)));
end );

InstallGlobalFunction( CoreFreeDegrees,
function(G)
	local subgroups;
	return Unique(List(CoreFreeConjugacyClassesSubgroups(G), n -> Index(G,n[1]));
end );
