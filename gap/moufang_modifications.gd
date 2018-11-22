############################################################################
##
#W  moufang_modifications.gd  Moufang modifications [loops]
##  
##  
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  LOOPS M(G,2)
##  -------------------------------------------------------------------------
DeclareGlobalFunction( "LoopMG2" );

#############################################################################
##  CYCLIC MODIFICATION
##  -------------------------------------------------------------------------
DeclareGlobalFunction( "LoopByCyclicModification" );

#############################################################################
##  DIHEDRAL MODIFICATION
##  -------------------------------------------------------------------------
DeclareGlobalFunction( "LoopByDihedralModification" );

#############################################################################
##  AUXILIARY
##  -------------------------------------------------------------------------
DeclareGlobalFunction( "LOOPS_PositionList" );
DeclareGlobalFunction( "LOOPS_Modular" );
DeclareGlobalFunction( "LOOPS_DVSigma" );
