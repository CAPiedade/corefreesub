## The function "Splash" is temporarily borrowed from Viz, while it is in an early stage of development.
## 
## (M. Delgado wrote this file during a visit I made to St Andrews University in January 2012)
#############################################################################

if not IsBound(VizOptionsForSplash) then 
  BindGlobal("VizOptionsForSplash",["viewtex","layout","path","directory","file","viewer","tikz","filetype"]);
fi;


#############################################################################
# Splash ... 
# the input is
# * a record of options (may not be present) and
# * a string (dot) or a function that applied to the ramaining argument produces a dot string
if not IsBound(VizViewers) then 
  if ARCH_IS_MAC_OS_X( ) then
    BindGlobal("VizViewers", ["xpdf","open","evince", "okular", "gv"]);
  elif ARCH_IS_UNIX( ) then
    BindGlobal("VizViewers", ["evince","xpdf","xdg-open","okular", "gv"]);
  elif ARCH_IS_WINDOWS( ) then
    BindGlobal("VizViewers", ["xpdf","evince", "okular", "gv"]);
  fi;
fi;

if not IsBound(Splash) then 
  BindGlobal("Splash",
  function(arg)
    local opt, dotstring, PermGroup, GeneralMap, G1, G2, path, dir, tdir, file, viewer, tikz, filetype, i, 
          latexstring, command, layout, viewtexfile, tmp;
    if IsList(arg[1]) then
      tmp := [];
      for i in arg do
        Append(tmp,i);
      od;
      arg := tmp;
    fi;
    ##########
    # there are global warnings concerning the avaiability of software
    # there is no need to put them here
    ###############
    opt := First(arg, k -> IsRecord(k));
    if opt = fail then
      opt := rec();
    else
      if not IsSubset(VizOptionsForSplash,RecNames( opt)) then
        Info(InfoDrawFTPR,1,"The options ", Difference(RecNames(opt),
         VizOptionsForSplash)," have no effect.");
      fi;
    fi;
    dotstring := First(arg, k -> IsString(k));
    if dotstring = fail then
      PermGroup := First(arg, k -> IsPermGroup(k));
      GeneralMap := First(arg, k -> IsGeneralMapping(k));
      G1 := First(arg, k -> IsGroup(k));
      G2 := First(arg, k -> IsGroup(k) and IsSubgroup(G1,k) and G1 <> k);
      if PermGroup <> fail and G2 = fail then dotstring := DotFTPRGraph(PermGroup);
      elif GeneralMap <> fail then dotstring := DotFTPRGraph(GeneralMap);
      elif G1 <> fail and G2 <> fail then dotstring := DotFTPRGraph(G1,G2);
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
        Exec(Concatenation("mkdir ",path,opt.directory));
        Info(InfoDrawFTPR, 1, "The temporary directory ",path,opt.directory, 
         " has been created");
      fi;
      dir := Concatenation(path,opt.directory,"/");
    elif IsBound(opt.path) then
      if not "tmp.viz" in DirectoryContents(path) then
        tdir := Directory(Concatenation(path,"/","tmp.viz"));
        dir := Filename(tdir, "");
      fi;
    else
      tdir := DirectoryTemporary();
      dir := Filename(tdir, "");
    fi;
    #
    Info(InfoDrawFTPR,2,"The temporary directory used is: ", dir,"\n");

    #file
    if IsBound(opt.file) then
      file := opt.file;
    else
      file := "vizpicture";
    fi;

    #viewer
    if IsBound(opt.viewer) then
      viewer := opt.viewer;
    else
      viewer := First(VizViewers, x ->
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
    if tikz or viewdrawntex then
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
