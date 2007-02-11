#############################################################################
##
#A  read.g                  loops                G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: read.g, v 1.4.0 2007/02/11 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  CORE METHODS
##  -------------------------------------------------------------------------
ReadPkg("loops", "gap/quasigrp.gi");

#############################################################################
##  LIBRARIES
##  -------------------------------------------------------------------------
ReadPkg("loops", "gap/examples.gi");

#############################################################################
##  SPECIFIC METHODS
##  -------------------------------------------------------------------------

# isomorphisms and automorphisms
ReadPkg("loops", "gap/loop_iso.gi"); 

# triality groups of Moufang loops
ReadPkg("loops", "gap/triality.gi"); 

# Moufang modifications
ReadPkg("loops", "gap/moufang_modifications.gi");
