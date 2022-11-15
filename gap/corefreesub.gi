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
	return Image(FactorCosetAction(G,Reversed(CoreFreeConjugacyClassesSubgroups(G))[1][1]));
end );

InstallOtherMethod( MinimalFaithfulTransitivePermutationRepresentation, [IsGroup, IsBool], 
function(G,bool)
	local core_free_ccs;
	if bool then
		core_free_ccs := Reversed(CoreFreeConjugacyClassesSubgroups(G));
		return List(Filtered(core_free_ccs, i -> Index(G,i[1]) = Index(G,core_free_ccs[1][1])), j -> Image(FactorCosetAction(G,j[1])));
	else
		return MinimalFaithfulTransitivePermutationRepresentation(G);
	fi;
end );


InstallGlobalFunction( MinimalFaithfulTransitivePermutationRepresentationDegree,
function(G)
	local n;
	return Minimum(List(CoreFreeConjugacyClassesSubgroups(G), n -> Index(G,n[1])));
end );


