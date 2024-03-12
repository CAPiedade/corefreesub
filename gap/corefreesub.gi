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
  G!.coreFreeDegrees := Unique(NiceO!.coreFreeDegrees);
  return G!.coreFreeConjugacyClassesSubgroups;
end );

BindGlobal( "CoreFreeConjugacyClassesSubgroupsSolvable",
  function(arg)
  local g,        # group
        isom,     # isomorphism onto AgSeries group
        func,     # automorphisms to be invariant under
        funcs,    # <func>
        funcnorm, # N_G(funcs)
        efunc,    # induced automs on factor
        efnorm,   # funcnorm^epi
        e,        # EAS
        len,      # Length(e)
        start,    # last index with EA factor
        i,j,k,l,
        m,kp,     # loop
        kgens,    # generators of k
        kconh,    # complemnt conjugacy storage
        opt,      # options record
        normal,   # flag for 'normal' option
        consider, # optional 'consider' function
        retnorm,  # option: return all normalizers
        f,        # g/e[i]
        home,     # HomePcgs(f)
        epi,      # g -> f
        lastepi,  # epi of last step
        n,        # e[i-1]^epi
        fa,       # f/n = g/e[i-1]
        hom,      # f -> fa
        B,        # subgroups of n
        ophom,    # perm action of f on B (or false if not computed)
        a,        # preimg. of group over n
        no,       # N_f(a)
  #      aop,     # a^ophom
  #      nohom,   # ophom\rest no
        oppcgs,   # acting pcgs
        oppcgsimg,# images under ophom
        ex,       # external set/orbits
        bs,       # b\in B normal under a, reps
        bsp,      # bs index
        bsnorms,  # respective normalizers
        b,        # in bs
        bpos,     # position in bs
        hom2,     # N_f(b) -> N_f(b)/b
        nag,      # AgGroup(n^hom2)
        fghom,    # assoc. epi
        t,s,      # dnk-transversals
        z,        # Cocycles
        coboundbas,# Basis(OneCobounds)
        field,    # GF(Exponent(n))
        com,      # complements
        comnorms, # normalizers supergroups
        isTrueComnorm, # is comnorms the true normalizer or a supergroup
        comproj,  # projection onto complement
        kgn,
        kgim,     # stored decompositions, translated to matrix language
        kgnr,     # assoc index
        ncom,     # dito, tested
        idmat,    # 1-matrix
        mat,      # matrix action
        mats,     # list of mats
        conj,     # matrix action
        chom,     # homom onto <conj>
        shom,     # by s induced autom
        shoms,    # list of these
        smats,    # dito, matrices
        conjnr,   # assoc. index
        glsyl,
        glsyr,    # left and right side of eqn system
        found,    # indicator for success
        grps,     # list of subgroups
        ngrps,    # dito, new level
        gj,       # grps[j]
        grpsnorms,# normalizers of grps
        ngrpsnorms,# dito, new level
        bgids,    # generators of b many 1's (used for copro)
        opr,      # operation on complements
        xo;       # xternal orbits

    g:=arg[1];
    if Length(arg)>1 and IsRecord(arg[Length(arg)]) then
      opt:=arg[Length(arg)];
    else
      opt:=rec();
    fi;

    # parse options
    retnorm:=IsBound(opt.retnorm) and opt.retnorm;

    # handle trivial case
    if IsTrivial(g) then
      if retnorm then
        return [[g],[g]];
      else
        return [g];
      fi;
    fi;

    normal:=IsBound(opt.normal) and opt.normal=true;
    if IsBound(opt.consider) then
      consider:=opt.consider;
    else
      consider:=false;
    fi;

    isom:=fail;

    # get automorphisms and compute their normalizer, if applicable
    if IsBound(opt.actions) then
      func:=opt.actions;
      hom2:= Filtered( func,     HasIsConjugatorAutomorphism
                            and IsInnerAutomorphism );
      hom2:= List( hom2, ConjugatorOfConjugatorIsomorphism );

      if IsBound(opt.funcnorm) then
        # get the func. normalizer
        funcnorm:=opt.funcnorm;
        b:=g;
      else
        funcs:= GroupByGenerators( Filtered( func,
                    i -> not ( HasIsConjugatorAutomorphism( i ) and
                              IsInnerAutomorphism( i ) ) ),
                    IdentityMapping(g));
        IsGroupOfAutomorphismsFiniteGroup(funcs); # set filter
        if IsTrivial( funcs ) then
          b:=ClosureGroup(Parent(g),List(func,ConjugatorOfConjugatorIsomorphism));
          func:=hom2;
        else
          if IsSolvableGroup(funcs) then
            a:=IsomorphismPcGroup(funcs);
          else
            a:=IsomorphismPermGroup(funcs);
          fi;
          hom:=InverseGeneralMapping(a);
          IsTotal(hom); IsSingleValued(hom); # to be sure (should be set anyway)
          b:=SemidirectProduct(Image(a),hom,g);
          hom:=Embedding(b,1);
          funcs:=List(GeneratorsOfGroup(funcs),i->Image(hom,Image(a,i)));
          isom:=Embedding(b,2);
          hom2:=List(hom2,i->Image(isom,i));
          func:=Concatenation(funcs,hom2);
          g:=Image(isom,g);
        fi;

        # get the normalizer of <func>
        funcnorm:=Normalizer(g,SubgroupNC(b,func));
        func:=List(func,i->ConjugatorAutomorphism(b,i));
      fi;

      Assert(1,IsSubgroup(g,funcnorm));

      # compute <func> characteristic series
      e:=InvariantElementaryAbelianSeries(g,func);
    else
      func:=[];
      funcnorm:=g;
      e:=ElementaryAbelianSeriesLargeSteps(g);
    fi;

    if IsBound(opt.series) then
      e:=opt.series;
    else
      f:=DerivedSeriesOfGroup(g);
      if Length(e)>Length(f) and
        ForAll([1..Length(f)-1],i->IsElementaryAbelian(f[i]/f[i+1])) then
        Info(InfoPcSubgroup,1,"  Preferring Derived Series");
        e:=f;
      fi;
    fi;

    len:=Length(e);

    if IsBound(opt.groups) then
      start:=0;
      while start+1<=Length(e) and ForAll(opt.groups,i->IsSubgroup(e[start+1],i)) do
        start:=start+1;
      od;
      Info(InfoPcSubgroup,1,"starting index ",start);
      epi:=NaturalHomomorphismByNormalSubgroup(g,e[start]);
      lastepi:=epi;
      f:=Image(epi,g);
      grps:=List(opt.groups,i->Image(epi,i));
      if not IsBound(opt.grpsnorms) then
        opt:=ShallowCopy(opt);
        opt.grpsnorms:=List(opt.groups,i->Normalizer(e[1],i));
      fi;
      grpsnorms:=List(opt.grpsnorms,i->Image(epi,i));
    else
      # search the largest elementary abelian quotient
      start:=2;
      while start<len and IsElementaryAbelian(g/e[start+1]) do
        start:=start+1;
      od;

      # compute all subgroups there
      if start<len then
        # form only factor groups if necessary
        epi:=NaturalHomomorphismByNormalSubgroup(g,e[start]);
        LockNaturalHomomorphismsPool(g,e[start]);
        f:=Image(epi,g);
      else
        f:=g;
        epi:=IdentityMapping(f);
      fi;
      lastepi:=epi;
      efunc:=List(func,i->InducedAutomorphism(epi,i));
      grps:=InvariantSubgroupsElementaryAbelianGroup(f,efunc);
      Assert(1,ForAll(grps,i->ForAll(efunc,j->Image(j,i)=i)));
      grpsnorms:=List(grps,i->f);
      Info(InfoPcSubgroup,5,List(grps,Size),List(grpsnorms,Size));

    fi;

    for i in [start+1..len] do
      Info(InfoPcSubgroup,1," step ",i,": ",Index(e[i-1],e[i]),", ",
                      Length(grps)," groups");
      # compute modulo e[i]
      if i<len then
        # form only factor groups if necessary
        epi:=NaturalHomomorphismByNormalSubgroup(g,e[i]);
        f:=Image(epi,g);
      else
        f:=g;
        epi:=IdentityMapping(g);
      fi;
      home:=HomePcgs(f); # we want to compute wrt. this pcgs
      n:=Image(epi,e[i-1]);

      # the induced factor automs
      efunc:=List(func,i->InducedAutomorphism(epi,i));
      # filter the non-trivial ones
      efunc:=Filtered(efunc,i->ForAny(GeneratorsOfGroup(f),j->Image(i,j)<>j));

      if Length(efunc)>0 then
        efnorm:=Image(epi,funcnorm);
      fi;

      if Length(efunc)=0 then
        ophom:=ActionSubspacesElementaryAbelianGroup(f,n);
        B:=ophom[1];
        Info(InfoPcSubgroup,2,"  ",Length(B)," normal subgroups");
        ophom:=ophom[2];

        ngrps:=[];
        ngrpsnorms:=[];
        oppcgs:=Pcgs(Source(ophom));
        oppcgsimg:=List(oppcgs,i->Image(ophom,i));
        ex:=[1..Length(B)];
        IsSSortedList(ex);
        ex:=ExternalSet(Source(ophom),ex,oppcgs,oppcgsimg,OnPoints);
        ex:=ExternalOrbitsStabilizers(ex);

        for j in ex do
          if IsCoreFree(g,PreImage(epi,B[Representative(j)])) then
            Add(ngrps,B[Representative(j)]);
            Add(ngrpsnorms,StabilizerOfExternalSet(j));
          fi;
        od;

      else
        B:=InvariantSubgroupsElementaryAbelianGroup(n,efunc);
        ophom:=false;
        Info(InfoPcSubgroup,2,"  ",Length(B)," normal subgroups");

        # note the groups in B
        ngrps:=SubgroupsOrbitsAndNormalizers(f,B,false);
        ngrpsnorms:=List(ngrps,i->i.normalizer);
        ngrps:=List(ngrps,i->i.representative);
        ngrps:=Filtered(ngrps,i->IsCoreFree(g,PreImage(epi,i)));
      fi;

      # Get epi to the old factor group
      # as hom:=NaturalHomomorphism(f,fa); does not work, we have to play tricks
      hom:=lastepi;
      lastepi:=epi;
      fa:=Image(hom,g);

      hom:= GroupHomomorphismByImagesNC(f,fa,GeneratorsOfGroup(f),
            List(GeneratorsOfGroup(f),i->
              Image(hom,PreImagesRepresentative(epi,i))));
      Assert(2,KernelOfMultiplicativeGeneralMapping(hom)=n);

      # lift the known groups
      for j in [1..Length(grps)] do

        gj:=grps[j];
        if Size(gj)>1 then
          a:=PreImage(hom,gj);
          Assert(1,Size(a)=Size(gj)*Size(n));
          if IsCoreFree(g,PreImage(epi,a)) then 
            Add(ngrps,a);
            Add(ngrpsnorms,PreImage(hom,grpsnorms[j]));
          fi;
          no:=PreImage(hom,grpsnorms[j]);

          if Length(efunc)>0 then
            # get the double cosets
            t:=List(DoubleCosets(f,no,efnorm),Representative);
            Info(InfoPcSubgroup,2,"  |t|=",Length(t));
            t:=Filtered(t,i->HasInvariantConjugateSubgroup(a,i,efunc));
            Info(InfoPcSubgroup,2,"invar:",Length(t));
          fi;

          # we have to extend with those b in B, that are normal in a
          if ophom<>false then
            #aop:=Image(ophom,a);
            #SetIsSolvableGroup(aop,true);

            if Length(GeneratorsOfGroup(a))>2 then
              bs:=SmallGeneratingSet(a);
            else
              bs:=GeneratorsOfGroup(a);
            fi;
            bs:=List(bs,i->Image(ophom,i));

            bsp:=Filtered([1..Length(B)],i->ForAll(bs,j->i^j=i)
                                          and Size(B[i])<Size(n));
            bs:=B{bsp};
          else
            bsp:=false;
            bs:=Filtered(B,i->IsNormal(a,i) and Size(i)<Size(n));
          fi;

          if Length(efunc)>0 and Length(t)>1 then
            # compute also the invariant ones under the conjugates:
            # equivalently: Take all equivalent ones and take those, whose
            # conjugates lie in a and are normal under a
            for k in Filtered(t,i->not i in no) do
              bs:=Union(bs,Filtered(List(B,i->ConjugateSubgroup(i,k^(-1))),
                    i->IsSubset(a,i) and IsNormal(a,i) and Size(i)<Size(n) ));
            od;
          fi;

          # take only those bs which are valid
          if consider<>false then
            Info(InfoPcSubgroup,2,"  ",Length(bs)," subgroups lead to ");
            if bsp<>false then
              bsp:=Filtered(bsp,j->consider(no,a,n,B[j],e[i])<>false);
              IsSSortedList(bsp);
              bs:=bsp; # to get the 'Info' right
            else
              bs:=Filtered(bs,j->consider(no,a,n,j,e[i])<>false);
            fi;
            Info(InfoPcSubgroup,2,Length(bs)," valid ones");
          fi;

          if ophom<>false then
            #nohom:=List(GeneratorsOfGroup(no),i->Image(ophom,i));
            #aop:=SubgroupNC(Image(ophom),nohom);
            #nohom:=GroupHomomorphismByImagesNC(no,aop,
            #                                   GeneratorsOfGroup(no),nohom);

            if Length(bsp)>0 then
              oppcgs:=Pcgs(no);
              oppcgsimg:=List(oppcgs,i->Image(ophom,i));
              ex:=ExternalSet(no,bsp,oppcgs,oppcgsimg,OnPoints);
              ex:=ExternalOrbitsStabilizers(ex);

              bs:=[];
              bsnorms:=[];
              for bpos in ex do
                Add(bs,B[Representative(bpos)]);
                Add(bsnorms,StabilizerOfExternalSet(bpos));
  
              od;
            fi;

          else
            # fuse under the action of no and compute the local normalizers
            bs:=SubgroupsOrbitsAndNormalizers(no,bs,true);
            bsnorms:=List(bs,i->i.normalizer);
            bs:=List(bs,i->i.representative);
          fi;

          Assert(1,ForAll(bs,i->ForAll(efunc,j->Image(j,i)=i)));

          # now run through the b in bs
          for bpos in [1..Length(bs)] do
            b:=bs[bpos];
            Assert(2,IsNormal(a,b));
            # test, whether we'll have to consider this case

  
            # test, whether b is invariant
            if Length(efunc)>0 then
              # extend to dcs of bnormalizer
              s:=RightTransversal(no,bsnorms[bpos]);
              nag:=Length(s);
              s:=Concatenation(List(s,i->List(t,j->i*j)));
              z:=Length(s);
              #NOCH: Fusion
              # test, which ones are usable at all
              s:=Filtered(s,i->HasInvariantConjugateSubgroup(b,i,efunc));
              Info(InfoPcSubgroup,2,"  |s|=",nag,"-(m)>",z,"-(i)>",Length(s));
            else
              s:=[()];
            fi;

            if Length(s)>0 then
              nag:=InducedPcgs(home,n);
              nag:=nag mod InducedPcgs(nag,b);


              z:=rec(group:=a,
                  generators:=InducedPcgs(home,a) mod NumeratorOfModuloPcgs(nag),
                  modulePcgs:=nag);
              OCOneCocycles(z,true);
              if IsBound(z.complement) and
                # normal complements exist, iff the coboundaries are trivial
                (normal=false or Dimension(z.oneCoboundaries)=0)
                then
                # now fetch the complements

                z.factorGens:=z.generators;
                coboundbas:=Basis(z.oneCoboundaries);
                com:=BaseSteinitzVectors(BasisVectors(Basis(z.oneCocycles)),
                                        BasisVectors(coboundbas));
                field:=LeftActingDomain(z.oneCocycles);
                if Size(field)^Length(com.factorspace)>100000 then
                  Info(InfoWarning,1, "Many (",
                    Size(field)^Length(com.factorspace),") complements!");
                fi;
                com:=Enumerator(VectorSpace(field,com.factorspace,
                                                Zero(z.oneCocycles)));
                Info(InfoPcSubgroup,3,"  ",Length(com),
                    " local complement classes");

                # compute fusion
                kconh:=List([1..Length(com)],i->[i]);
                if i<len or retnorm then
                  # we need to compute normalizers
                  comnorms:=[];
                else
                  comnorms:=fail;
                fi;

                if Length(com)>1 and Size(a)<Size(bsnorms[bpos]) then

                  opr:=function(cyc,elm)
                        local l,i;
                          l:=z.cocycleToList(cyc);
                          for i in [1..Length(l)] do
                            l[i]:=(z.complementGens[i]*l[i])^elm;
                          od;
                          l:=CorrespondingGeneratorsByModuloPcgs(z.origgens,l);
                          for i in [1..Length(l)] do
                            l[i]:=LeftQuotient(z.complementGens[i],l[i]);
                          od;
                          l:=z.listToCocycle(l);
                          return SiftedVector(coboundbas,l);
                        end;

                  xo:=ExternalOrbitsStabilizers(
                      ExternalSet(bsnorms[bpos],com,opr));

                  for k in xo do
                    l:=List(k,i->Position(com,i));
                    if comnorms<>fail then
                      comnorms[l[1]]:=StabilizerOfExternalSet(k);
                      isTrueComnorm:=false;
                    fi;
                    l:=Set(l);
                    for kp in l do
                      kconh[kp]:=l;
                    od;
                  od;

                elif comnorms<>fail then
                  if Size(a)=Size(bsnorms[bpos]) then
                    comnorms:=List(com,i->z.cocycleToComplement(i));
                    isTrueComnorm:=true;
                    comnorms:=List(comnorms,
                                i->ClosureSubgroup(CentralizerModulo(n,b,i),i));
                  else
                    isTrueComnorm:=false;
                    comnorms:=List(com,i->bsnorms[bpos]);
                  fi;
                fi;


                if Length(efunc)>0 then
                  ncom:=[];


                  conj:=LinearOperationLayer(a,GeneratorsOfGroup(a),nag);

                  idmat:=conj[1]^0;
                  mat:= GroupByGenerators( conj, idmat );
                  chom:= GroupHomomorphismByImagesNC(a,mat,
                          GeneratorsOfGroup(a),conj);

                  smats:=[];
                  shoms:=[];

                  fghom:=Concatenation(z.factorGens,GeneratorsOfGroup(n));
                  bgids:=List(GeneratorsOfGroup(n),i->One(b));

                  # now run through the complements
                  for kp in [1..Length(com)] do

                    if kconh[kp]=fail then
                      Info(InfoPcSubgroup,3,"already conjugate");
                    else

                      l:=z.cocycleToComplement(com[kp]);
                      # the projection on the complement
                      k:=ClosureSubgroup(b,l);
                      if Length(s)=1 and IsOne(s[1]) then
                        # special case -- no conjugates
                        if ForAll(efunc,x->ForAll(GeneratorsOfGroup(l),
                            y->ImagesRepresentative(x,y) in k)) then
                          l:=rec(representative:=k);
                          if comnorms<>fail then
                            if IsBound(comnorms[kp]) then
                              l.normalizer:=comnorms[kp];
                            else
                              l.normalizer:=Normalizer(bsnorms[bpos],
                                      ClosureSubgroup(b,k));
                            fi;
                          fi;
                          Add(ncom,l);

                          # tag all conjugates
                          for l in kconh[kp] do
                            kconh[l]:=fail;
                          od;
                        fi;

                      else
                        # generic case

                        comproj:= GroupHomomorphismByImagesNC(a,a,fghom,
                                  Concatenation(GeneratorsOfGroup(l),bgids));

                        # now run through the conjugating elements
                        conjnr:=1;
                        found:=false;
                        while conjnr<=Length(s) and found=false do
                          if not IsBound(smats[conjnr]) then
                            # compute the matrix action for the induced, jugated
                            # morphisms
                            m:=s[conjnr];
                            smats[conjnr]:=[];
                            shoms[conjnr]:=[];
                            for l in efunc do
                              # the induced, jugated morphism
                              shom:= GroupHomomorphismByImagesNC(a,a,
                                      GeneratorsOfGroup(a),
                                      List(GeneratorsOfGroup(a),
                                      i->Image(l,i^m)^Inverse(m)));

                              mat:=List(nag,
                                    i->One(field)*ExponentsOfPcElement(nag,
                                    Image(shom,i)));
                              Add(smats[conjnr],mat);
                              Add(shoms[conjnr],shom);
                            od;
                          fi;

                          mats:=smats[conjnr];
                          # now test whether the complement k can be conjugated to
                          # be invariant under the morphisms to mats
                          glsyl:=List(nag,i->[]);
                          glsyr:=[];
                          for l in [1..Length(efunc)] do
                            kgens:=GeneratorsOfGroup(k);
                            for kgnr in [1..Length(kgens)] do

                              kgn:=Image(shoms[conjnr][l],kgens[kgnr]);
                              kgim:=Image(comproj,kgn);
                              Assert(2,kgim^-1*kgn in n);
                              # nt part
                              kgn:=kgim^-1*kgn;

                              # translate into matrix terms
                              kgim:=Image(chom,kgim);
                              kgn:=One(field)*ExponentsOfPcElement(nag,kgn);

                              # the matrix action
                              mat:=idmat+(mats[l]-idmat)*kgim-mats[l];

                              # store action and vector
                              for m in [1..Length(glsyl)] do
                                glsyl[m]:=Concatenation(glsyl[m],mat[m]);
                              od;
                              glsyr:=Concatenation(glsyr,kgn);

                            od;
                          od;

                          # a possible conjugating element is a solution of the
                          # large LGS
                          l:= SolutionMat(glsyl,glsyr);
                          if l <> fail then
                            m:=Product([1..Length(l)],
                                      i->nag[i]^IntFFE(l[i]));
                            # note that we found one!
                            found:=[s[conjnr],m];
                          fi;

                          conjnr:=conjnr+1;
                        od;

                        # there is an invariant complement?
                        if found<>false then
                          found:=found[2]*found[1];
                          l:=ConjugateSubgroup(ClosureSubgroup(b,k),found);
                          Assert(1,ForAll(efunc,i->Image(i,l)=l));
                          l:=rec(representative:=l);
                          if comnorms<>fail then
                            if IsBound(comnorms[kp]) then
                              l.normalizer:=ConjugateSubgroup(comnorms[kp],found);
                            else
                              l.normalizer:=ConjugateSubgroup(
                                              Normalizer(bsnorms[bpos],
                                      ClosureSubgroup(b,k)), found);
                            fi;
                          fi;
                          Add(ncom,l);

                          # tag all conjugates
                          for l in kconh[kp] do
                            kconh[l]:=fail;
                          od;

                        fi;

                      fi;

                    fi; # if not already a conjugate

                  od;

                  # if invariance test needed
                else
                  # get representatives of the fused complement classes
                  l:=Filtered([1..Length(com)],i->kconh[i][1]=i);

                  ncom:=[];
                  for kp in l do
                    m:=rec(representative:=
                            ClosureSubgroup(b,z.cocycleToComplement(com[kp])));
                    if comnorms<>fail then
                      m.normalizer:=comnorms[kp];
                    fi;
                    Add(ncom,m);
                  od;
                fi;
                com:=ncom;

                # take the preimages
                for k in com do

                  Assert(1,ForAll(efunc,i->Image(i,k.representative)
                                          =k.representative));
                  if IsCoreFree(g,PreImage(epi,k.representative)) then
                    Add(ngrps,k.representative);
                    if IsBound(k.normalizer) then
                      if isTrueComnorm then
                        Add(ngrpsnorms,k.normalizer);
                      else
                        Add(ngrpsnorms,Normalizer(k.normalizer,k.representative));
                      fi;
                    fi;
                  fi;
                od;
              fi;
            fi;
          od;

        fi;
      od;

      grps:=ngrps;
      grpsnorms:=ngrpsnorms;
      Info(InfoPcSubgroup,5,List(grps,Size),List(grpsnorms,Size));
    od;

    if isom<>fail then
      grps:=List(grps,j->PreImage(isom,j));
      if retnorm then
        grpsnorms:=List(grpsnorms,j->PreImage(isom,j));
      fi;
    fi;

    if retnorm then
      return [grps,grpsnorms];
    else
      return grps;
    fi;

end );

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
  s:=CoreFreeConjugacyClassesSubgroupsSolvable(GI,rec(retnorm:=true));
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

