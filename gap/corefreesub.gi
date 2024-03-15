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
  AUTODOC_file_scan_list := [ "../PackageInfo.g", "../gap/CF_splashfromViz.g", "../gap/corefreesub.gd", "../gap/corefreesub.gi", "../gap/drawftpr.gd", "../gap/drawftpr.gi", "../init.g", "../makedoc.g", "../maketest.g", "../read.g", Filename(DirectoriesPackageLibrary("corefreesub", "doc"),"_Chunks.xml") ];
  LoadPackage( "GAPDoc" );
  doctests := ExtractExamples( DirectoriesPackageLibrary("corefreesub", "doc"), "corefreesub.xml", AUTODOC_file_scan_list, 500 );
  RunExamples( doctests, rec( compareFunction := "uptowhitespace" ) );
  Test(Filename(DirectoriesPackageLibrary( "corefreesub", "tst" )[1],"OtherTest.g"));
end);


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
  return G!.coreFreeConjugacyClassesSubgroups;
end );

BindGlobal( "CoreFreeConjugacyClassesSubgroupsOfSolvableGroup",
  #### The function "CoreFreeConjugacyClassesSubgroupsOfSolvableGroup" is a modified version from "FiniteSubgroupClassesBySeries" 
  #### from "Polycyclic" version 2.16 GAP Package, under GNU General Public License v2 or above.
    function(G)
    local s,i,c,k,CoreFreeClasses,map,GI, PCPGI, map2, pcps,
     pcpG, grps, grp, cfgrps, pcp, rels, act, new, newcore, C, tmp ;

    if not IsPcGroup(G) or IsPermGroup(G) then
        map:=IsomorphismPcGroup(G);
        GI:=Image(map,G);
    else
        map:=fail;
        GI:=G;
    fi;
    map2 := IsomorphismPcpGroup(GI);
    PCPGI := Image(map2,GI);
    
    pcps:= PcpsOfEfaSeries(PCPGI);

    pcpG := Pcp( PCPGI );
    grps := [ rec( repr := PCPGI , norm := PCPGI) ];
    cfgrps := [];
    for pcp in pcps do
        rels := RelativeOrdersOfPcp( pcp );
        act := OperationAndSpaces( pcpG, pcp );
        new := [];
        newcore := [];
        for i in [1..Length( grps ) ] do
            grp := grps[i];
            Info( InfoPcpGrp, 1, "  group number ", i );

            # set up class record
            C := rec( );
            C.group  := grp.repr;
            C.super  := Pcp( grp.norm, grp.repr );
            C.factor := Pcp( grp.repr, GroupOfPcp( pcp ) );
            C.normal := pcp;

            # add extension info
            AddFieldCR( C );
            AddRelatorsCR( C );

            # add action
            TranslateAction( C, pcpG, act.mats );

            # if it is free abelian, compute complements
            if C.char = 0 then
                AddInversesCR( C );
                tmp := ComplementClassesCR( C );
                Info( InfoPcpGrp, 1, "  computed ", Length(tmp),
                    " complements");
            else
                if IsBound( act.spaces ) then C.spaces := act.spaces; fi;
                tmp := SupplementClassesCR( C );
                Info( InfoPcpGrp, 1, "  computed ", Length(tmp),
                    " supplements");
            fi;

            Append( new, tmp );
            for k in [1 .. Size(tmp)] do
              if IsCoreFree(GI,PreImage(map2,tmp[k].repr)) then
                Append( newcore, [tmp[k]] );
              fi;
            od;
        od;
        Append( cfgrps, newcore);
        if C.char = 0 then
            grps := ShallowCopy( new );
        else
            Append( grps, new );
        fi;
    od;


    CoreFreeClasses := [];
    # translate to classes and return

    for i in [1..Length(cfgrps)] do
        if map=fail then
        c:=ConjugacyClassSubgroups(G,PreImage(map2,cfgrps[i].repr));
        SetStabilizerOfExternalSet(c,PreImage(map2,cfgrps[i].norm));
        Add(CoreFreeClasses,c);
        else
        c:=ConjugacyClassSubgroups(G,PreImage(map,PreImage(map2,cfgrps[i].repr)));
        SetStabilizerOfExternalSet(c,PreImage(map,PreImage(map2,cfgrps[i].norm)));
        Add(CoreFreeClasses,c);
        fi;
    od;
    

    Sort(CoreFreeClasses,function(a,b)
        return Size(Representative(a))<Size(Representative(b));
    end);
    G!.coreFreeConjugacyClassesSubgroups := CoreFreeClasses;
    return G!.coreFreeConjugacyClassesSubgroups;
end);

BindGlobal("CoreFreeSubgroupsTrivialFitting",function(G)
  local s,a,n,fac,iso,types,t,p,i,map,go,gold,nf,tom,sub,len,pos_list_cf;

  n:=DirectFactorsFittingFreeSocle(G);

  # is it almost simple and stored?
  if Length(n)=1 then
    tom:=TomDataAlmostSimpleRecognition(G);
    if tom<>fail then
      go:=ImagesSource(tom[1]);
      len:=LengthsTom(tom[2]);
      pos_list_cf := Filtered([1..Length(len)], x -> IsCoreFree(go,RepresentativeTom(tom[2],x)));
      sub:=List(pos_list_cf,x->PreImage(tom[1],RepresentativeTom(tom[2],x)));
      return sub;
    fi;
  fi;

  s:=Socle(G);

  a:=TrivialSubgroup(G);
  fac:=[];
  nf:=[];
  types:=[];
  gold:=[];
  iso:=[];
  for i in n do
    if not IsSubgroup(a,i) then
      a:=ClosureGroup(a,i);
      if not IsNonabelianSimpleGroup(i) then
        Error("The subgroups are abelian or not simple groups in CoreFreeSubgroupsTrivialFitting");
      fi;
      t:=ClassicalIsomorphismTypeFiniteSimpleGroup(i);
      p:=Position(types,t);
      if p=fail then
        Add(types,t);

        # fetch subgroup data from tom library, if possible
        tom:=TomDataAlmostSimpleRecognition(i);
        if tom<>fail then
          go:=ImagesSource(tom[1]);
          if tom[2]<>fail then
            len:=LengthsTom(tom[2]);
            # different than above -- no preimage. We're setting subgroups
            # of go
            sub:=List([1..Length(len)],x->RepresentativeTom(tom[2],x));
            sub:=List(sub,x->ConjugacyClassSubgroups(go,x));
            SetConjugacyClassesSubgroups(go,sub);
          fi;
        fi;

        if tom=fail then
          go:=SimpleGroup(t);
        fi;
        Add(gold,go);

        p:=Length(types);
      fi;
      Add(iso,IsomorphismGroups(i,gold[p]));
      Add(fac,gold[p]);
      Add(nf,i);
    fi;
  od;

  if a<>s then
    Error("The closure group is not trivial in CoreFreeSubgroupsTrivialFitting");
  fi;

  if Length(fac)=1 then
    map:=iso[1];
    a:=CoreFreeConjugacyClassesSubgroups(gold[1]);
    a:=List(a,x->PreImage(map,Representative(x)));
  else
    n:=DirectProduct(fac);

    # map to direct product
    a:=[];
    map:=[];
    for i in [1..Length(fac)] do
      Append(a,GeneratorsOfGroup(nf[i]));
      Append(map,List(GeneratorsOfGroup(nf[i]),
        x->Image(Embedding(n,i),Image(iso[i],x))));
    od;
    map:=GroupHomomorphismByImages(s,n,a,map);

    a:=SubdirectSubgroups(n);
    a:=List(a,x->PreImage(map,x[1]));
  fi;

  s:=Filtered(ExtendSubgroupsOfNormal(G,s,a), i -> IsCoreFree(G,i));
  return s;
end);

BindGlobal( "CoreFreeConjugacyClassesSubgroupsViaRadical",
function(G)
  local H,HN,HNI,ser,pcgs,u,hom,f,c,nu,nn,nf,a,e,kg,k,mpcgs,gf,
  act,nts,orbs,n,ns,nim,fphom,as,p,isns,lmpc,npcgs,ocr,v,
  com,cg,i,j,w,ii,first,cgs,presmpcgs,select,fselect,
  makesubgroupclasses,cefastersize,cfccs, cfccsdeg, new_nu;

  #group order below which cyclic extension is usually faster
  # WORKAROUND: there is a disparity between the data format returned
  # by CE and what this code expects. This could be resolved properly,
  # but since most people will have tomlib loaded anyway, this doesn't
  # seem worth the effort.
  #if IsPackageMarkedForLoading("tomlib","")=true then
    cefastersize:=1;
  #else
  #  cefastersize:=40000;
  #fi;

  makesubgroupclasses:=function(g,l)
  local i,m,c;
    m:=[];
    for i in l do
      c:=ConjugacyClassSubgroups(g,i);
      if IsBound(i!.GNormalizer) then
        SetStabilizerOfExternalSet(c,i!.GNormalizer);
        Unbind(i!.GNormalizer);
      fi;
      Add(m,c);
    od;
    return m;
  end;

  ser:=PermliftSeries(G:limit:=300); # do not form too large spaces as they
                                     # clog up memory
  pcgs:=ser[2];
  ser:=ser[1];
  if Index(G,ser[1])=1 then
    Info(InfoWarning,3,"group is solvable");
    hom:=NaturalHomomorphismByNormalSubgroup(G,G);
    hom:=hom*IsomorphismFpGroup(Image(hom));
    u:=[[G],[G],[hom]];
    cfccs := [];
    cfccsdeg := [];
  elif Size(ser[1])=1 then
    if (HasIsSimpleGroup(G) and IsSimpleGroup(G)) then
      if IsNonabelianSimpleGroup(G) then
        c:=TomDataSubgroupsAlmostSimple(G);
        if c<>fail then
          if Size(c[Size(c)]) = Size(G) then
            c:= c{[1..Size(c)-1]};
          else
            Remove(c,Position(c,G));
          fi;
          c:=makesubgroupclasses(G,c);
          G!.coreFreeConjugacyClassesSubgroups := c;
          return G!.coreFreeConjugacyClassesSubgroups;
        fi;
      fi;
      
      return CoreFreeConjugacyClassesSubgroupsCyclicExtension(G);
    else
      if Length(ser) = 1 then
        c:=CoreFreeSubgroupsTrivialFitting(G);
        cfccs:=makesubgroupclasses(G,c);
        if not ForAny(c,x->Size(x)=1) then Add(cfccs,ConjugacyClassSubgroups(G,TrivialSubgroup(G))); fi;
        G!.coreFreeConjugacyClassesSubgroups := cfccs;
        return G!.coreFreeConjugacyClassesSubgroups;
      fi;
      c:=SubgroupsTrivialFitting(G);
      c:=makesubgroupclasses(G,c);
      u:=[List(c,Representative),List(c,StabilizerOfExternalSet)];
      cfccs := CoreFreeSubgroupsTrivialFitting(G);
    fi;
  else
    hom:=NaturalHomomorphismByNormalSubgroupNC(G,ser[1]);
    f:=Image(hom,G);
    c:=SubgroupsTrivialFitting(f);
    c:=makesubgroupclasses(f,c);
    nu:=[];
    nn:=[];
    nf:=[];
    kg:=GeneratorsOfGroup(KernelOfMultiplicativeGeneralMapping(hom));
    for i in c do
      a:=Representative(i);
      #k:=PreImage(hom,a);
      # make generators of homomorphism fit nicely to presentation
      gf:=IsomorphismFpGroup(a);
      e:=List(MappingGeneratorsImages(gf)[1],x->PreImagesRepresentative(hom,x));
      # we cannot guarantee that the parent contains e, so no
      # ClosureSubgroup.
      k:=ClosureGroup(KernelOfMultiplicativeGeneralMapping(hom),e);
      Add(nu,k);
      Add(nn,PreImage(hom,Stabilizer(i)));
      Add(nf,GroupHomomorphismByImagesNC(k,Range(gf),Concatenation(e,kg),
             Concatenation(MappingGeneratorsImages(gf)[2],
                List(kg,x->One(Range(gf))))));
    od;
    u:=[nu,nn,nf];
    cfccs := makesubgroupclasses(G,Filtered(u[1], i -> IsCoreFree(G,i)));
    cfccsdeg := List(cfccs, i -> Index(G,i[1]));
  fi;


  for i in [2..Length(ser)] do
    Info(InfoLattice,1,"Step ",i," : ",Index(ser[i-1],ser[i]));
    if pcgs=false then
      mpcgs:=ModuloPcgs(ser[i-1],ser[i]);
    else
      mpcgs:=pcgs[i-1] mod pcgs[i];
    fi;
    presmpcgs:=mpcgs;

    if Length(mpcgs)>0 then
      gf:=GF(RelativeOrders(mpcgs)[1]);
      act:=ActionSubspacesElementaryAbelianGroup(G,mpcgs);
    else
      gf:=GF(Factors(Index(ser[i-1],ser[i]))[1]);
      act:=[[ser[i]],GroupHomomorphismByImagesNC(G,Group(()),
           GeneratorsOfGroup(G),
           List(GeneratorsOfGroup(G),i->()))];
    fi;
    nts:=act[1];
    act:=act[2];
    if IsGroupGeneralMappingByImages(act) then
      Size(Source(act));
      Size(Range(act));
    fi;
    nu:=[];
    nn:=[];
    nf:=[];
    # Determine which ones we need and keep old ones
    orbs:=[];
    for j in [1..Length(u[1])] do
      a:=u[1][j];
      n:=u[2][j];

      # find indices of subgroups normal under a and form orbits under the
      # normalizer
      if act<>fail then
        ns:=Difference([1..Length(nts)],MovedPoints(Image(act,a)));
        nim:=Image(act,n);
        ns:=Orbits(nim,ns);
      else
        nim:=Filtered([1..Length(nts)],x->IsNormal(a,nts[x]));
        ns:=[];
        for k in [1..Length(nim)] do
          if not ForAny(ns,x->nim[k] in x) then
            p:=Orbit(n,nts[k]);
            p:=List(p,x->Position(nts,x));
            p:=Filtered(p,x->x<>fail and x in nim);
            Add(ns,p);
          fi;
        od;
      fi;
      if Size(a)>Size(ser[i-1]) then
        # keep old groups
        if IsSubset(HN,a) then
          Add(nu,a);Add(nn,n);
          if Size(ser[i])>1 then
            fphom:=LiftFactorFpHom(u[3][j],a,ser[i],presmpcgs);
            Add(nf,fphom);
          fi;
        fi;
        orbs[j]:=ns;
      else # here a is the trivial subgroup in the factor. (This will never
           # happen if we look for perfect or simple groups!)
        orbs[j]:=[];
        # previous kernel -- there the orbits are classes of subgroups in G
        for k in ns do
          Add(nu,nts[k[1]]);
          Add(nn,PreImage(act,Stabilizer(nim,k[1])));
          if Size(ser[i])>1 then
            fphom:=IsomorphismFpGroupByChiefSeriesFactor(nts[k[1]],"x",ser[i]);
            Add(nf,fphom);
          fi;
        od;
      fi;
    od;

    # run through nontrivial subspaces (greedy test whether they are needed)
    for j in [1..Length(nts)] do
      if Size(nts[j])<Size(ser[i-1]) then
        as:=[];
        for k in [1..Length(orbs)] do
          p:=PositionProperty(orbs[k],z->j in z);
          if p<>fail then
            # remove orbit
            orbs[k]:=orbs[k]{Difference([1..Length(orbs[k])],[p])};
            Add(as,k);
          fi;
        od;
        if Length(as)>0 then
          Info(InfoLattice,2,"Normal subgroup ",j,", Size ",Size(nts[j]),": ",
               Length(as)," subgroups to consider");
          # there are subgroups that will complement with this kernel.
          # Construct the modulo pcgs and the action of the largest subgroup
          # (which must be the normalizer)
          isns:=1;
          for k in as do
            if Size(u[1][k])>isns then
              isns:=Size(u[1][k]);
            fi;
          od;

          if pcgs=false then
            lmpc:=ModuloPcgs(ser[i-1],nts[j]);
            if Size(nts[j])=1 and Size(ser[i])=1 then
              # avoid degenerate case
              npcgs:=Pcgs(nts[j]);
            else
              npcgs:=ModuloPcgs(nts[j],ser[i]);
            fi;
          else
            if IsTrivial(nts[j]) then
              lmpc:=pcgs[i-1];
              npcgs:="not used";
            else
              c:=InducedPcgs(pcgs[i-1],nts[j]);
              lmpc:=pcgs[i-1] mod c;
              npcgs:=c mod pcgs[i];
            fi;
          fi;

          for k in as do
            a:=u[1][k];
            if IsNormal(u[2][k],nts[j]) then
              n:=u[2][k];
            else
              n:=Normalizer(u[2][k],nts[j]);
            fi;
            if Length(GeneratorsOfGroup(n))>3 then
              w:=Size(n);
              n:=Group(SmallGeneratingSet(n));
              SetSize(n,w);
            fi;
            ocr:=rec(group:=a,
                    modulePcgs:=lmpc);
            ocr.factorfphom:=u[3][k];

            OCOneCocycles(ocr,true);
            if IsBound(ocr.complement) then
              v:=BaseSteinitzVectors(
                BasisVectors(Basis(ocr.oneCocycles)),
                BasisVectors(Basis(ocr.oneCoboundaries)));
              v:=VectorSpace(gf,v.factorspace,Zero(ocr.oneCocycles));
              com:=[];
              cgs:=[];
              first:=false;
              if Size(v)>100 and Size(ser[i])=1
                 and HasElementaryAbelianFactorGroup(a,nts[j]) then
                com:=VectorspaceComplementOrbitsLattice(n,a,ser[i-1],nts[j]);
                Info(InfoLattice,4,"Subgroup ",Position(as,k),"/",Length(as),
                      ", ",Size(v)," local complements, ",Length(com)," orbits");
                for c in com do
                  if H=fail or IsSubset(HN,c.representative) then
                    Add(nu,c.representative);
                    Add(nn,c.normalizer);
                  fi;
                od;
              else
                for w in Enumerator(v) do
                  cg:=ocr.cocycleToList(w);
                  for ii in [1..Length(cg)] do
                    cg[ii]:=ocr.complementGens[ii]*cg[ii];
                  od;
                  if first then
                    # this is clearly kept -- so calculate a stabchain
                    c:=ClosureSubgroup(nts[j],cg);
                  first:=false;
                  else
                    c:=SubgroupNC(G,Concatenation(SmallGeneratingSet(nts[j]),cg));
                  fi;
                  Assert(1,Size(c)=Index(a,ser[i-1])*Size(nts[j]));
                  if H=fail or IsSubset(HN,c) then
                    SetSize(c,Index(a,ser[i-1])*Size(nts[j]));
                    Add(cgs,cg);
                    #c!.comgens:=cg;
                    Add(com,c);
                  fi;
                od;
                w:=Length(com);
                com:=SubgroupsOrbitsAndNormalizers(n,com,false:savemem:=true);
                Info(InfoLattice,3,"Subgroup ",Position(as,k),"/",Length(as),
                      ", ",w," local complements, ",Length(com)," orbits");
                for w in com do
                  c:=w.representative;
                  if fselect=fail or fselect(c) then
                    Add(nu,c);
                    Add(nn,w.normalizer);
                    if Size(ser[i])>1 then
                      # need to lift presentation
                      fphom:=ComplementFactorFpHom(ocr.factorfphom,
                      ser[i-1],nts[j],c,
                      ocr.generators,cgs[w.pos]);

                      Assert(1,KernelOfMultiplicativeGeneralMapping(fphom)=nts[j]);
                      if Size(nts[j])>Size(ser[i]) then
                        fphom:=LiftFactorFpHom(fphom,c,ser[i],npcgs);
                        Assert(1,
                          KernelOfMultiplicativeGeneralMapping(fphom)=ser[i]);
                      fi;
                      Add(nf,fphom);
                    fi;
                  fi;

                od;
              fi;

              ocr:=false;
              cgs:=false;
              com:=false;
            fi;
          od;
        fi;
      fi;
    od;

    u:=[nu,nn,nf];
    new_nu := Filtered(nu, j -> not true in List(cfccs, k -> j in k) );
    Append(cfccs, makesubgroupclasses(G,Filtered(new_nu, i -> IsCoreFree(G,i))));
    cfccs := Unique(cfccs);
    cfccsdeg := List(cfccs, i -> Index(G,i[1]));
  od;
  
  # some `select'ions remove the trivial subgroup
  if not ForAny(cfccs,x->Size(x[1])=1) then
    Add(cfccs,ConjugacyClassSubgroups(G,TrivialSubgroup(G)));
  fi;
  Sort(cfccs,function(a,b)
    return Size(Representative(a))<Size(Representative(b));
  end);
  G!.coreFreeConjugacyClassesSubgroups := cfccs;
  return G!.coreFreeConjugacyClassesSubgroups;
end);


#############################################################################################

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
      classes := List(CoreFreeConjugacyClassesSubgroups(H), i -> ConjugacyClassSubgroups(G,PreImage(iso,Representative(i))) );
      G!.coreFreeConjugacyClassesSubgroups := classes;
      G!.coreFreeDegrees := H!.coreFreeDegrees;
      return G!.coreFreeConjugacyClassesSubgroups;
    elif CanComputeFittingFree(G) then
      return CoreFreeConjugacyClassesSubgroupsViaRadical(G);
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
  local n, cfd;
  if IsBound(G!.coreFreeDegrees) then
    return G!.coreFreeDegrees;
  elif IsBound(G!.coreFreeConjugacyClassesSubgroups) then
    G!.coreFreeDegrees := Unique(List(G!.coreFreeConjugacyClassesSubgroups, n -> Index(G,n[1])));
    return G!.coreFreeDegrees;
  else
    cfd := Unique(List(CoreFreeConjugacyClassesSubgroups(G), n -> Index(G,n[1])));
    Sort(cfd);
    G!.coreFreeDegrees := cfd;
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

InstallMethod( FaithfulTransitivePermutationRepresentationsOfDegree, [IsGroup, IsInt], 
        function(G,ind)
  if not ind in CoreFreeDegrees(G) then
    Error("There are no Core-free subgroups with index ", ind);
  fi;
  return FactorCosetAction(G,First(CoreFreeConjugacyClassesSubgroups(G), j -> Index(G,j[1]) = ind)[1]);
end );

InstallOtherMethod( FaithfulTransitivePermutationRepresentationsOfDegree, [IsGroup, IsInt, IsBool], 
        function(G,ind,bool) #bool here is either "we want all FTPRs" - true, or "only one for each degree"
  if not ind in CoreFreeDegrees(G) then
    Error("There are no Core-free subgroups with index ", ind);
  fi;
  if bool then
    return List(Filtered(CoreFreeConjugacyClassesSubgroups(G), j -> Index(G,j[1]) = ind), i -> FactorCosetAction(G,i[1]));
  else
    return FaithfulTransitivePermutationRepresentationsOfDegree(G);
  fi;
end );

