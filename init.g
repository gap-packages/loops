#############################################################################
##
#A  init.g                  loops                G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: init.g, v 2.0.0 2008/03/06 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  DECLARING PACKAGE
##  -------------------------------------------------------------------------
DeclareAutoPackage("loops", "2.0.0", ReturnTrue);

#############################################################################
##  BANNER
##  -------------------------------------------------------------------------
ReadPkg("loops", "gap/banner.g");

#############################################################################
##  METHODS FOR ALL QUASIGROUPS AND LOOPS
##  -------------------------------------------------------------------------
ReadPkg( "loops", "gap/quasigroups.gd");    # representing, creating and displaying quasigroups
ReadPkg( "loops", "gap/elements.gd");       # elements and basic arithmetic operations
ReadPkg( "loops", "gap/core_methods.gd");   # most common structural methods
ReadPkg( "loops", "gap/classes.gd");        # testing varieties         
ReadPkg( "loops", "gap/iso.gd");            # isomorphisms and isotopisms
ReadPkg( "loops", "gap/extensions.gd");     # extensions

#############################################################################
##  METHODS FOR BOL AND MOUFANG LOOPS
##  -------------------------------------------------------------------------
ReadPkg( "loops", "gap/bol_core_methods.gd");       # most common methods for Bol loops
ReadPkg( "loops", "gap/moufang_triality.gd" );      # triality for Moufang loops
ReadPkg( "loops", "gap/moufang_modifications.gd");  # modifications of Moufang loops

#############################################################################
##  LIBRARIES
##  -------------------------------------------------------------------------
ReadPkg( "loops", "gap/examples.gd");     # libraries

#############################################################################
##  HELP
##  -------------------------------------------------------------------------
ReadPkg( "loops", "etc/HBHforLOOPS.g");   # the handler functions for GAP's help system
