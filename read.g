#############################################################################
##
#A  read.g                  loops                G. P. Nagy / P. Vojtechovsky
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  METHODS FOR ALL QUASIGROUPS AND LOOPS
##  -------------------------------------------------------------------------
ReadPackage( "loops", "gap/quasigroups.gi" );    # representing, creating and displaying quasigroups
ReadPackage( "loops", "gap/elements.gi" );       # elements and basic arithmetic operations
ReadPackage( "loops", "gap/core_methods.gi" );   # most common structural methods
ReadPackage( "loops", "gap/classes.gi" );        # testing properties of loops
ReadPackage( "loops", "gap/iso.gi" );            # isomorphisms and isotopisms
ReadPackage( "loops", "gap/extensions.gi" );     # extensions
ReadPackage( "loops", "gap/random.gi" );         # random loops
ReadPackage( "loops", "gap/mlt_search.gi" );     # realizing loops as multiplication groups of loops

#############################################################################
##  METHODS FOR BOL AND MOUFANG LOOPS
##  -------------------------------------------------------------------------
ReadPackage( "loops", "gap/bol_core_methods.gi" );          # most common methods for Bol loops
ReadPackage( "loops", "gap/moufang_triality.gi" );          # triality for Moufang loops
ReadPackage( "loops", "gap/moufang_modifications.gi" );     # modifications of Moufang loops

#############################################################################
##  LIBRARIES
##  -------------------------------------------------------------------------
ReadPackage( "loops", "gap/convert.gi" );       # conversions between numerical bases and encoding of Cayley tables 
ReadPackage( "loops", "gap/examples.gi");       # libraries of loops

#############################################################################
##  MEMORY MANAGEMENT
##  -------------------------------------------------------------------------
ReadPackage( "loops", "gap/memory.gi" );        # memory management


