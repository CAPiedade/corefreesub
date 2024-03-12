## The function "Splash" is temporarily borrowed from Viz, while it is in an early stage of development.
## 
## (M. Delgado wrote this file during a visit made to St Andrews University in January 2012)
#############################################################################

if not IsBound(CF_VizOptionsForSplash) then 
  BindGlobal("CF_VizOptionsForSplash",["viewtexfile","layout","path","directory","file","viewer","tikz","filetype", "gen_name"]);
fi;


#############################################################################
# Splash ... 
# the input is
# * a record of options (may not be present) and
# * a string (dot) or a function that applied to the remaining argument produces a dot string
if not IsBound(CF_VizViewers) then 
  if ARCH_IS_MAC_OS_X( ) then
    BindGlobal("CF_VizViewers", ["xpdf","open","evince", "okular", "gv"]);
  elif ARCH_IS_UNIX( ) then
    BindGlobal("CF_VizViewers", ["evince","xpdf","xdg-open","okular", "gv"]);
  elif ARCH_IS_WINDOWS( ) then
    BindGlobal("CF_VizViewers", ["xpdf","evince", "okular", "gv"]);
  fi;
fi;

if not IsBound(CF_Splash) then 
  BindGlobal("CF_Splash",
  function(arg)
    local opt, dotstring, PermGroup, GeneralMap, G1, G2, path, dir, tdir, file, viewer, tikz, filetype, i, 
          latexstring, command, layout, viewtexfile, tmp, gen_name, eachdir;
    if IsList(arg[1]) then
      tmp := [];
      for i in arg do
        Append(tmp,i);
      od;
      arg := tmp;
    fi;
    ##########
    # there are global warnings concerning the availability of software
    # there is no need to put them here
    ###############
    opt := First(arg, k -> IsRecord(k));
    if opt = fail then
      opt := rec();
    else
      if not IsSubset(CF_VizOptionsForSplash,RecNames( opt)) then
        Info(InfoDrawFTPR,1,"The options ", Difference(RecNames(opt),
         CF_VizOptionsForSplash)," have no effect.");
      fi;
    fi;


    dotstring := First(arg, k -> IsString(k));
    if dotstring = fail then
      PermGroup := First(arg, k -> IsPermGroup(k));
      GeneralMap := First(arg, k -> IsGeneralMapping(k));
      G1 := First(arg, k -> IsGroup(k));
      G2 := First(arg, k -> IsGroup(k) and IsSubgroup(G1,k) and G1 <> k);
      if IsBound(opt.gen_name) then
        gen_name := opt.gen_name;
        if PermGroup <> fail and G2 = fail  then dotstring := DotFTPRGraph(PermGroup, gen_name);
        elif GeneralMap <> fail then dotstring := DotFTPRGraph(GeneralMap, gen_name);
        elif G1 <> fail and G2 <> fail then dotstring := DotFTPRGraph(G1,G2, gen_name);
        fi;
      else
        if PermGroup <> fail and G2 = fail  then dotstring := DotFTPRGraph(PermGroup);
        elif GeneralMap <> fail then dotstring := DotFTPRGraph(GeneralMap);
        elif G1 <> fail and G2 <> fail then dotstring := DotFTPRGraph(G1,G2);
        fi;
      fi;
    fi;
    # begin options
    #path
    if IsBound(opt.path) then
      path := opt.path;
    else
      path := "~/";
    fi;

    #directory
    if IsBound(opt.directory) then
      if not opt.directory in DirectoryContents(path) then
        dir := "";
        for eachdir in SplitString(opt.directory, "/") do
          Add(eachdir,'/');
          Append(dir,eachdir);
          Exec(Concatenation("mkdir ",path,dir,"/"));
        od;
          Info(InfoDrawFTPR, 2, "The temporary directory ",path,dir, 
          " has been created");
          tdir := Directory(Concatenation(path,dir));
      else
        tdir := Directory(Concatenation(path,opt.directory,"/"));
      fi; 
      dir := Filename(tdir, "");
    elif IsBound(opt.path) and not IsBound(opt.directory) then
      if not "tmp.viz" in DirectoryContents(path) then
        if path[Size(path)] <> '/' then
          tdir := Directory(Concatenation(path,"/","tmp.viz"));
          dir := Filename(tdir, "");
        else
          tdir := Directory(Concatenation(path,"/","tmp.viz"));
          dir := Filename(tdir, "");
        fi;
      fi;
    else
      tdir := DirectoryTemporary();
      dir := Filename(tdir, "");
    fi;
    #
    Info(InfoDrawFTPR,2,"The directory used is: ", dir,"\n");

    #file
    if IsBound(opt.file) then
      file := opt.file;
    else
      file := "CF_FTPR";
    fi;

    #viewer
    if IsBound(opt.viewer) then
      viewer := opt.viewer;
    else
      viewer := First(CF_VizViewers, x ->
       Filename(DirectoriesSystemPrograms(),x)<>fail);
    fi;

    # latex
    if IsBound(opt.tikz) then
      tikz := opt.tikz;
    else
      tikz := false;
    fi;
    if IsBound(opt.filetype) then
      filetype := opt.filetype;
    else
      filetype := "pdf";
    fi;

    # layout
    if IsBound(opt.layout) and opt.layout in ["dot", "neato", "twopi", "circo", "fdp", "sfdp", "patchwork", "osage"] then
      layout := opt.layout;
    elif IsBound(opt.layout) and not opt.layout in ["dot", "neato", "twopi", "circo", "fdp", "sfdp", "patchwork", "osage"] and not tikz then
      Info(InfoDrawFTPR,1,"The layout is not supported. Using neato instead.\n");
      layout := "neato";
    else
      layout := "neato";
    fi;

    #texview
    if IsBound(opt.viewtexfile) then
      viewtexfile := opt.viewtexfile;
    else
      viewtexfile := false;
    fi;
    ######################
    if tikz or viewtexfile then
      FileString(Concatenation(dir,file,".dot"),dotstring);
      command := Concatenation("dot2tex --prog=",layout," -ftikz ",dir,file,".dot"," > ",
        dir,file,".tex");
      Exec(command);
      if viewtexfile then
        return ReadAll(InputTextFile(Concatenation(dir,file,".tex")));
      fi;

      command := Concatenation("cd ",dir,"; ","pdflatex ",dir,file, 
      " 2>/dev/null 1>/dev/null");
      Exec(command);

      Exec (Concatenation(viewer, " ",dir,file,".pdf 2>/dev/null 1>/dev/null &"));
      return;
    fi;

    FileString(Concatenation(dir,file,".dot"),dotstring);
    command := Concatenation(layout," -T",filetype," ",dir,file,".dot"," -o ", dir,file,".",filetype);
    Exec(command);
    Exec (Concatenation(viewer, " ",dir,file,".",filetype," 2>/dev/null 1>/dev/null &"));
    return;
  end);
fi;
