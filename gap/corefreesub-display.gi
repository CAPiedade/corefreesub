##########################################################################
##########################################################################
##
# The aim of this function is to "splash" an image directly from the dot code.
# To this effect, it adds a preamble and makes a call to the Viz Splash function.
# To avoid forcing the user to install the Viz package (under development), a copy of the Viz Splash function is included in the file "splash_from_viz.g" of this package

InstallGlobalFunction(CoreFreeSub_Splash,
        function(arg)
  local  opt, dotstr;
  
  opt := First(arg, IsRecord);
  dotstr := First(arg, IsString);
  
    Splash(dotstr,opt);  
end);
