#############################################################################
##
#A  read.g                  loops                G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: read.g, v 2.1.1 2009/12/30 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  METHODS FOR ALL QUASIGROUPS AND LOOPS
##  -------------------------------------------------------------------------
ReadPkg( "loops", "gap/quasigroups.gi");    # representing, creating and displaying quasigroups
ReadPkg( "loops", "gap/elements.gi");       # elements and basic arithmetic operations
ReadPkg( "loops", "gap/core_methods.gi");   # most common structural methods
ReadPkg( "loops", "gap/classes.gi");        # testing properties of loops
ReadPkg( "loops", "gap/iso.gi");            # isomorphisms and isotopisms
ReadPkg( "loops", "gap/extensions.gi");     # extensions
ReadPkg( "loops", "gap/random.gi");         # random loops
ReadPkg( "loops", "gap/mlt_search.gi");     # realizing loops as multiplication groups of loops

#############################################################################
##  METHODS FOR BOL AND MOUFANG LOOPS
##  -------------------------------------------------------------------------
ReadPkg( "loops", "gap/bol_core_methods.gi");       # most common methods for Bol loops
ReadPkg( "loops", "gap/moufang_triality.gi" );      # triality for Moufang loops
ReadPkg( "loops", "gap/moufang_modifications.gi");  # modifications of Moufang loops

#############################################################################
##  LIBRARIES
##  -------------------------------------------------------------------------
ReadPkg( "loops", "gap/examples.gi");     # libraries

