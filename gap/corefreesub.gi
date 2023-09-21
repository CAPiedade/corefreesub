#
# corefreesub: A GAP Package for calculating the core-free subgroups and their faithful transitive permutation representations
#
# Implementations
#
#
#
### Testing Functions
BindGlobal( "CF_TESTALL",
  function()
	local tests, doctests, AUTODOC_file_scan_list;
  AUTODOC_file_scan_list := [ "../PackageInfo.g", "../gap/CF_splashfromViz.g", "../gap/corefreesub.gd", "../gap/corefreesub.gi", "../gap/drawftpr.gd", "../gap/drawftpr.gi", "../init.g", "../makedoc.g", "../maketest.g", "../read.g", "/mnt/c/Users/Claudio/GAP Packages/pkg/corefreesub/doc/_Chunks.xml" ];
  LoadPackage( "GAPDoc" );
  doctests := ExtractExamples( DirectoriesPackageLibrary("corefreesub", "doc"), "corefreesub.xml", AUTODOC_file_scan_list, 500 );
  RunExamples( doctests, rec( compareFunction := "uptowhitespace" ) );
	tests := Filtered(DirectoryContents(DirectoriesPackageLibrary("corefreesub", "tst")[1]), i -> i = "testall.g");
	if tests = [] then
    Error("Test file not in corefreesub/tst/ folder");
  else 
    Read(Filename(DirectoriesPackageLibrary("corefreesub", "tst"), "testall.g"));
  fi;
end);



BindGlobal( "CoreFreeConjugacyClassesSubgroupsOfSolvableGroup",
        function(G)
  local s,i,c,CoreFreeClasses,CoreFreeDegrees,map,GI;

  if not IsPcGroup(G) or IsPermGroup(G) then
    map:=IsomorphismPcGroup(G);
    GI:=Image(map,G);
  else
    map:=fail;
    GI:=G;
  fi;
  s:=SubgroupsSolvableGroup(GI,rec(retnorm:=true));
  CoreFreeClasses:=[];
  CoreFreeDegrees := [];
  for i in [1..Length(s[1])] do
    if map=fail and IsCoreFree(GI,s[1][i]) then
      c:=ConjugacyClassSubgroups(GI,s[1][i]);
      SetStabilizerOfExternalSet(c,s[2][i]);
      Add(CoreFreeClasses,c);
      Add(CoreFreeDegrees,Index(GI,s[1][i]));
    elif IsCoreFree(GI,s[1][i]) then
      c:=ConjugacyClassSubgroups(G,PreImage(map,s[1][i]));
      SetStabilizerOfExternalSet(c,PreImage(map,s[2][i]));
      Add(CoreFreeClasses,c);
      Add(CoreFreeDegrees,Index(GI,s[1][i]));
    fi;
  od;
  Sort(CoreFreeClasses,function(a,b)
    return Size(Representative(a))<Size(Representative(b));
  end);
  Sort(CoreFreeDegrees);
  G!.coreFreeConjugacyClassesSubgroups := CoreFreeClasses;
  G!.coreFreeDegrees := Unique(CoreFreeDegrees);
  return [G!.coreFreeConjugacyClassesSubgroups,G!.coreFreeDegrees][1];
end);

SOLVABILITY_IMPLYING_FUNCTIONS:=
  [IsSolvableGroup,IsNilpotentGroup,IsPGroup,IsCyclic];

BindGlobal("CoreFreeConjugacyClassesSubgroupsCyclicExtension",
        function(arg)
  local G,                 # group
        func,              # test function
        zuppofunc,         # test fct for zuppos
        noperf,            # discard perfect groups
        lattice,           # lattice (result)
        factors,           # factorization of <G>'s size
        zuppos,            # generators of prime power order
        zupposPrime,       # corresponding prime
        zupposPower,       # index of power of generator
        ZupposSubgroup,    # function to compute zuppos for subgroup
        zuperms,           # permutation of zuppos by group
        Gimg,              # grp image under zuperms
        nrClasses,         # number of classes
        classes,           # list of all classes
        classesZups,       # zuppos blist of classes
        classesExts,       # extend-by blist of classes
        perfect,           # classes of perfect subgroups of <G>
        perfectNew,        # this class of perfect subgroups is new
        perfectZups,       # zuppos blist of perfect subgroups
        layerb,            # begin of previous layer
        layere,            # end of previous layer
        H,                 # representative of a class
        Hzups,             # zuppos blist of <H>
        Hexts,             # extend blist of <H>
        C,                 # class of <I>
        I,                 # new subgroup found
        Ielms,             # elements of <I>
        Izups,             # zuppos blist of <I>
        N,                 # normalizer of <I>
        Nzups,             # zuppos blist of <N>
        Jzups,             # zuppos of a conjugate of <I>
        Kzups,             # zuppos of a representative in <classes>
        reps,              # transversal of <N> in <G>
        corefreedegrees,
        ac,
        transv,
        factored,
        mapped,
        expandmem,
        h,i,k,l,ri,rl,r;      # loop variables

  G:=arg[1];
  noperf:=false;
  zuppofunc:=false;
  if Length(arg)>1 and (IsFunction(arg[2]) or IsList(arg[2])) then
    func:=arg[2];
    Info(InfoLattice,1,"lattice discarding function active!");
    if IsList(func) then
      zuppofunc:=func[2];
      func:=func[1];
    fi;
    if Length(arg)>2 and IsBool(arg[3]) then
      noperf:=arg[3];
    fi;
  else
    func:=false;
  fi;

  expandmem:=ValueOption("Expand")=true;

  # if store is true, an element list will be kept in `Ielms' if possible
  ZupposSubgroup:=function(U,store)
    local elms,zups;
    if Size(U)=Size(G) then
      if store then Ielms:=fail;fi;
      zups:=BlistList([1..Length(zuppos)],[1..Length(zuppos)]);
    elif Size(U)>10^4 then
      # the group is very big - test the zuppos with `in'
      Info(InfoLattice,3,"testing zuppos with `in'");
      if store then Ielms:=fail;fi;
      zups:=List(zuppos,i->i in U);
      IsBlist(zups);
    else
      elms:=AsSSortedListNonstored(U);
      if store then Ielms:=elms;fi;
      zups:=BlistList(zuppos,elms);
    fi;
    return zups;
  end;

  # compute the factorized size of <G>
  factors:=Factors(Size(G));

  # compute a system of generators for the cyclic sgr. of prime power size
  if zuppofunc<>false then
    zuppos:=Zuppos(G,zuppofunc);
  else
    zuppos:=Zuppos(G);
  fi;

  Info(InfoLattice,1,"<G> has ",Length(zuppos)," zuppos");

  # compute zuppo permutation
  if IsPermGroup(G) then
    zuppos:=List(zuppos,SmallestGeneratorPerm);
    zuppos:=AsSSortedList(zuppos);
    zuperms:=List(GeneratorsOfGroup(G),
                  i->Permutation(i,zuppos,function(x,a)
      return SmallestGeneratorPerm(x^a);
    end));
    if NrMovedPoints(zuperms)<200*NrMovedPoints(G) then
      zuperms:=GroupHomomorphismByImagesNC(G,Group(zuperms),
                       GeneratorsOfGroup(G),zuperms);
      # force kernel, also enforces injective setting
      Gimg:=Image(zuperms);
      if Size(KernelOfMultiplicativeGeneralMapping(zuperms))=1 then
        SetSize(Gimg,Size(G));
      fi;
    else
      zuperms:=fail;
    fi;
  else
    zuppos:=AsSSortedList(zuppos);
    zuperms:=fail;
  fi;

  # compute the prime corresponding to each zuppo and the index of power
  zupposPrime:=[];
  zupposPower:=[];
  for r  in zuppos  do
    i:=SmallestRootInt(Order(r));
    Add(zupposPrime,i);
    k:=0;
    while k <> false  do
      k:=k + 1;
      if GcdInt(i,k) = 1  then
        l:=Position(zuppos,r^(i*k));
        if l <> fail  then
          Add(zupposPower,l);
          k:=false;
        fi;
      fi;
    od;
  od;
  Info(InfoLattice,1,"powers computed");

  if func<>false and
     (noperf or func in SOLVABILITY_IMPLYING_FUNCTIONS) then
    Info(InfoLattice,1,"Ignoring perfect subgroups");
    perfect:=[];
  else
    if IsPermGroup(G) then
      # trigger potentially better methods
      IsNaturalSymmetricGroup(G);
      IsNaturalAlternatingGroup(G);
    fi;
    perfect:=RepresentativesPerfectSubgroups(G);
    perfect:=Filtered(perfect,i->Size(i)>1 and Size(i)<Size(G));
    if func<>false then
      perfect:=Filtered(perfect,func);
    fi;
    perfect:=List(perfect,i->AsSubgroup(Parent(G),i));
  fi;

  perfectZups:=[];
  perfectNew :=[];
  for i  in [1..Length(perfect)]  do
    I:=perfect[i];
    #perfectZups[i]:=BlistList(zuppos,AsSSortedListNonstored(I));
    perfectZups[i]:=ZupposSubgroup(I,false);
    perfectNew[i]:=true;
  od;
  Info(InfoLattice,1,"<G> has ",Length(perfect),
       " representatives of perfect subgroups");

  # initialize the classes list
  nrClasses:=1;
  classes:=ConjugacyClassSubgroups(G,TrivialSubgroup(G));
  SetSize(classes,1);
  classes:=[classes];
  corefreedegrees := [Size(G)];
  classesZups:=[BlistList(zuppos,[One(G)])];
  classesExts:=[DifferenceBlist(BlistList(zuppos,zuppos),classesZups[1])];
  layerb:=1;
  layere:=1;

  # loop over the layers of group (except the group itself)
  for l  in [1..Length(factors)-1]  do
    Info(InfoLattice,1,"doing layer ",l,",",
         "previous layer has ",layere-layerb+1," classes");

    # extend representatives of the classes of the previous layer
    for h  in [layerb..layere]  do

      # get the representative,its zuppos blist and extend-by blist
      H:=Representative(classes[h]);
      Hzups:=classesZups[h];
      Hexts:=classesExts[h];
      Info(InfoLattice,2,"extending subgroup ",h,", size = ",Size(H));

      # loop over the zuppos whose <p>-th power lies in <H>
      for i  in [1..Length(zuppos)]  do

        if Hexts[i] and Hzups[zupposPower[i]]  then

          # make the new subgroup <I>
          # NC is safe -- all groups are subgroups of Parent(H)
          I:=ClosureSubgroupNC(H,zuppos[i]);
          #Subgroup(Parent(G),Concatenation(GeneratorsOfGroup(H),
          #                           [zuppos[i]]));
          if IsCoreFree(G,I) and (func=false or func(I)) then

            SetSize(I,Size(H) * zupposPrime[i]);

            # compute the zuppos blist of <I>
            #Ielms:=AsSSortedListNonstored(I);
            #Izups:=BlistList(zuppos,Ielms);
            if zuperms=fail then
              Izups:=ZupposSubgroup(I,true);
            else
              Izups:=ZupposSubgroup(I,false);
            fi;

            # compute the normalizer of <I>
            N:=Normalizer(G,I);
            #AH 'NormalizerInParent' attribute ?
            Info(InfoLattice,2,"found new class ",nrClasses+1,
                 ", size = ",Size(I)," length = ",Size(G)/Size(N));

            # make the new conjugacy class
            C:=ConjugacyClassSubgroups(G,I);
            SetSize(C,Size(G) / Size(N));
            SetStabilizerOfExternalSet(C,N);
            nrClasses:=nrClasses + 1;
            classes[nrClasses]:=C;
            Add(corefreedegrees,Index(G,I));

            # store the extend by list
            if l < Length(factors)-1  then
              classesZups[nrClasses]:=Izups;
              #Nzups:=BlistList(zuppos,AsSSortedListNonstored(N));
              Nzups:=ZupposSubgroup(N,false);
              SubtractBlist(Nzups,Izups);
              classesExts[nrClasses]:=Nzups;
            fi;

            # compute the right transversal
            # (but don't store it in the parent)
            if expandmem and zuperms<>fail then
              if Index(G,N)>400 then
                ac:=AscendingChainOp(G,N); # do not store
                while Length(ac)>2 and Index(ac[3],ac[1])<100 do
                  ac:=Concatenation([ac[1]],ac{[3..Length(ac)]});
                od;
                if Length(ac)>2 and
                   Maximum(List([3..Length(ac)],x->Index(ac[x],ac[x-1])))<500
                   then

                  # mapped factorized transversal
                  Info(InfoLattice,3,"factorized transversal ",
                       List([2..Length(ac)],x->Index(ac[x],ac[x-1])));
                  transv:=[];
                  ac[Length(ac)]:=Gimg;
                  for ri in [Length(ac)-1,Length(ac)-2..1] do
                    ac[ri]:=Image(zuperms,ac[ri]);
                    if ri=1 then
                      transv[ri]:=List(RightTransversalOp(ac[ri+1],ac[ri]),
                                       i->Permuted(Izups,i));
                    else
                      transv[ri]:=AsList(RightTransversalOp(ac[ri+1],ac[ri]));
                    fi;
                  od;
                  mapped:=true;
                  factored:=true;
                  reps:=Cartesian(transv);
                  Unbind(ac);
                  Unbind(transv);
                else
                  reps:=RightTransversalOp(Gimg,Image(zuperms,N));
                  mapped:=true;
                  factored:=false;
                fi;
              else
                reps:=RightTransversalOp(G,N);
                mapped:=false;
                factored:=false;
              fi;
            else
              reps:=RightTransversalOp(G,N);
              mapped:=false;
              factored:=false;
            fi;

            # loop over the conjugates of <I>
            for ri in [1..Length(reps)] do
              CompletionBar(InfoLattice,3,"Coset loop: ",ri/Length(reps));
              r:=reps[ri];

              # compute the zuppos blist of the conjugate
              if zuperms<>fail then
                # we know the permutation of zuppos by the group
                if mapped then
                  if factored then
                    Jzups:=r[1];
                    for rl in [2..Length(r)] do
                      Jzups:=Permuted(Jzups,r[rl]);
                    od;
                  else
                    Jzups:=Permuted(Izups,r);
                  fi;
                else
                  if factored then
                    Error("factored");
                  else
                    Jzups:=Image(zuperms,r);
                    Jzups:=Permuted(Izups,Jzups);
                  fi;
                fi;
              elif r = One(G)  then
                Jzups:=Izups;
              elif Ielms<>fail then
                Jzups:=BlistList(zuppos,OnTuples(Ielms,r));
              else
                Jzups:=ZupposSubgroup(I^r,false);
              fi;

              # loop over the already found classes
              for k  in [h..layere]  do
                Kzups:=classesZups[k];

                # test if the <K> is a subgroup of <J>
                if IsSubsetBlist(Jzups,Kzups)  then
                  # don't extend <K> by the elements of <J>
                  SubtractBlist(classesExts[k],Jzups);
                fi;

              od;

            od;
            CompletionBar(InfoLattice,3,"Coset loop: ",false);

            # now we are done with the new class
            Unbind(Ielms);
            Unbind(reps);
            Info(InfoLattice,2,"tested inclusions");

          else
            Info(InfoLattice,1,"discarded!");
          fi; # if condition fulfilled

        fi; # if Hexts[i] and Hzups[zupposPower[i]]  then ...
      od; # for i  in [1..Length(zuppos)]  do ...

      # remove the stuff we don't need any more
      Unbind(classesZups[h]);
      Unbind(classesExts[h]);
    od; # for h  in [layerb..layere]  do ...

    # add the classes of perfect subgroups
    for i  in [1..Length(perfect)]  do
      if    perfectNew[i]
            and IsPerfectGroup(perfect[i])
            and Length(Factors(Size(perfect[i]))) = l
            and IsCoreFree(G,perfect[i])
            then

        # make the new subgroup <I>
        I:=perfect[i];

        # compute the zuppos blist of <I>
        #Ielms:=AsSSortedListNonstored(I);
        #Izups:=BlistList(zuppos,Ielms);
        if zuperms=fail then
          Izups:=ZupposSubgroup(I,true);
        else
          Izups:=ZupposSubgroup(I,false);
        fi;

        # compute the normalizer of <I>
        N:=Normalizer(G,I);
        # AH: NormalizerInParent ?
        Info(InfoLattice,2,"found perfect class ",nrClasses+1,
             " size = ",Size(I),", length = ",Size(G)/Size(N));

        # make the new conjugacy class
        C:=ConjugacyClassSubgroups(G,I);
        SetSize(C,Size(G)/Size(N));
        SetStabilizerOfExternalSet(C,N);
        nrClasses:=nrClasses + 1;
        classes[nrClasses]:=C;
        Add(corefreedegrees,Index(G,I));
        # store the extend by list
        if l < Length(factors)-1  then
          classesZups[nrClasses]:=Izups;
          #Nzups:=BlistList(zuppos,AsSSortedListNonstored(N));
          Nzups:=ZupposSubgroup(N,false);
          SubtractBlist(Nzups,Izups);
          classesExts[nrClasses]:=Nzups;
        fi;

        # compute the right transversal
        # (but don't store it in the parent)
        reps:=RightTransversalOp(G,N);

        # loop over the conjugates of <I>
        for r  in reps  do

          # compute the zuppos blist of the conjugate
          if zuperms<>fail then
            # we know the permutation of zuppos by the group
            Jzups:=Image(zuperms,r);
            Jzups:=Permuted(Izups,Jzups);
          elif r = One(G)  then
            Jzups:=Izups;
          elif Ielms<>fail then
            Jzups:=BlistList(zuppos,OnTuples(Ielms,r));
          else
            Jzups:=ZupposSubgroup(I^r,false);
          fi;

          # loop over the perfect classes
          for k  in [i+1..Length(perfect)]  do
            Kzups:=perfectZups[k];

            # throw away classes that appear twice in perfect
            if Jzups = Kzups  then
              perfectNew[k]:=false;
              perfectZups[k]:=[];
            fi;

          od;

        od;

        # now we are done with the new class
        Unbind(Ielms);
        Unbind(reps);
        Info(InfoLattice,2,"tested equalities");

        # unbind the stuff we dont need any more
        perfectZups[i]:=[];

      fi;
      # if IsPerfectGroup(I) and Length(Factors(Size(I))) = layer the...
    od; # for i  in [1..Length(perfect)]  do

    # on to the next layer
    layerb:=layere+1;
    layere:=nrClasses;

  od; # for l  in [1..Length(factors)-1]  do ...

  # add the whole group to the list of classes
  Info(InfoLattice,1,"doing layer ",Length(factors),",",
       " previous layer has ",layere-layerb+1," classes");

  # return the list of classes
  Info(InfoLattice,1,"<G> has ",nrClasses," classes,",
       " and ",Sum(classes,Size)," subgroups");
  Sort(corefreedegrees);
  Sort(classes,function(a,b)
    return Size(Representative(a))<Size(Representative(b));
  end);
  G!.coreFreeConjugacyClassesSubgroups := classes;
  G!.coreFreeDegrees := Unique(corefreedegrees);
  return G!.coreFreeConjugacyClassesSubgroups;
end);

BindGlobal( "CoreFreeConjugacyClassesSubgroupsNiceMonomorphism",
        function(G)
  local hom, classes,NiceO;
  hom:= NiceMonomorphism( G );
  NiceO := NiceObject(G);
  classes:= List( CoreFreeConjugacyClassesSubgroups( NiceO ),
                  C -> ConjugacyClassSubgroups( G,
                          PreImage( hom, Representative( C ) ) ) );
  Sort(classes,function(a,b)
    return Size(Representative(a))<Size(Representative(b));
  end);
  G!.coreFreeConjugacyClassesSubgroups := classes;
  G!.coreFreeDegrees := Unique(NiceO!.coreFreeDegrees);
  return G!.coreFreeConjugacyClassesSubgroups;
end );

InstallGlobalFunction( IsCoreFree,
        function(G,H)
  if IsSubgroup(G,H) then   
    if Size(Core(G,H)) = 1 then return true; else return false; fi; 
  else
    Error("The subgroup given is not a subgroup of the given group");
  fi;
end);

InstallGlobalFunction( CoreFreeConjugacyClassesSubgroups,
        function(G)
  local iso, H, n, i, classes;
  if IsGroup(G) and IsFinite(G) then
    if IsBound(G!.coreFreeConjugacyClassesSubgroups) then
      return G!.coreFreeConjugacyClassesSubgroups;
    elif IsTrivial(G) then
      G!.coreFreeConjugacyClassesSubgroups := [ConjugacyClassSubgroups(G,G)];
      G!.coreFreeDegrees := [1];
      return G!.coreFreeConjugacyClassesSubgroups;
    elif HasIsHandledByNiceMonomorphism(G) then
      return CoreFreeConjugacyClassesSubgroupsNiceMonomorphism(G);
    elif IsSolvableGroup(G) then
      return CoreFreeConjugacyClassesSubgroupsOfSolvableGroup(G);
    elif not IsPermGroup(G) then 
      iso := IsomorphismPermGroup(G);
      H := Image(iso);
      classes := List(Filtered(CoreFreeConjugacyClassesSubgroupsCyclicExtension(H), n -> IsCoreFree(H,n[1])), i -> ConjugacyClassSubgroups(G,PreImage(iso,Representative(i))) );
      G!.coreFreeConjugacyClassesSubgroups := classes;
      G!.coreFreeDegrees := H!.coreFreeDegrees;
      return G!.coreFreeConjugacyClassesSubgroups;
    else
      return CoreFreeConjugacyClassesSubgroupsCyclicExtension(G);
    fi;
  elif IsGroup(G) and not IsFinite(G) then
    Error("Group should be finite");
  else
    Error("Argument has to be a Group"); 
  fi;
end );




InstallGlobalFunction( AllCoreFreeSubgroups,
        function(G)
  local i, j;
  if IsBound(G!.coreFreeConjugacyClassesSubgroups) then
    return Flat(List(G!.coreFreeConjugacyClassesSubgroups, i -> List(i, j -> j)));
  else
    return Flat(List(CoreFreeConjugacyClassesSubgroups(G), i -> List(i, j -> j)));
  fi;
end );

InstallGlobalFunction( CoreFreeDegrees,
        function(G)
  local n;
  if IsBound(G!.coreFreeDegrees) then
    return G!.coreFreeDegrees;
  elif IsBound(G!.coreFreeConjugacyClassesSubgroups) then
    G!.coreFreeDegrees := Unique(List(G!.coreFreeConjugacyClassesSubgroups, n -> Index(G,n[1])));
    return G!.coreFreeDegrees;
  else
    G!.coreFreeDegrees := Unique(List(CoreFreeConjugacyClassesSubgroups(G), n -> Index(G,n[1])));
    return G!.coreFreeDegrees;
  fi;
end );


InstallMethod( MinimalFaithfulTransitivePermutationRepresentation, [IsGroup], 
        function(G)
  local core_free_ccs;
  if Index(G,Reversed(CoreFreeConjugacyClassesSubgroups(G))[1][1]) = MinimumList(CoreFreeDegrees(G)) then
    return FactorCosetAction(G,Reversed(CoreFreeConjugacyClassesSubgroups(G))[1][1]);
  else 
    Error("Core-free subgroups are not being ordered correctly");
  fi;
end );

InstallOtherMethod( MinimalFaithfulTransitivePermutationRepresentation, [IsGroup, IsBool], 
        function(G,bool) #bool here is either "we want all minimal degree FTPRs" - true , or "only one of the minimal degree FTPR"
  local core_free_ccs;
  if bool then
    core_free_ccs := Reversed(CoreFreeConjugacyClassesSubgroups(G));
    return List(Filtered(core_free_ccs, i -> Index(G,i[1]) = MinimumList(CoreFreeDegrees(G)) ), j -> FactorCosetAction(G,j[1]));
  else
    return MinimalFaithfulTransitivePermutationRepresentation(G);
  fi;
end );


InstallGlobalFunction( MinimalFaithfulTransitivePermutationDegree,
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

