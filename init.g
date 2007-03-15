#############################################################################
##
#A  init.g                  loops                G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: init.g, v 1.5.0 2007/03/15 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  DECLARING PACKAGE
##  -------------------------------------------------------------------------
DeclareAutoPackage("loops", "1.5.0", ReturnTrue);

#############################################################################
##  BANNER
##  -------------------------------------------------------------------------
ReadPkg("loops", "gap/banner.g");

#############################################################################
##  CORE METHODS
##  -------------------------------------------------------------------------
ReadPkg( "loops", "gap/quasigrp.gd");               

#############################################################################
##  LIBRARIES
##  -------------------------------------------------------------------------
ReadPkg( "loops", "gap/examples.gd");               

#############################################################################
##  SPECIFIC METHODS
##  -------------------------------------------------------------------------

# isomorphisms and automorphisms
ReadPkg( "loops", "gap/loop_iso.gd"); 

# extensions
ReadPkg( "loops", "gap/extensions.gd");

# triality groups of Moufang loops
ReadPkg( "loops", "gap/triality.gd");               

# modifications of Moufang loops
ReadPkg( "loops", "gap/moufang_modifications.gd");

# the handler functions for GAP's help system
ReadPkg( "loops", "etc/HBHforLOOPS.g");
