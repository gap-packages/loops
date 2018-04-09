#############################################################################
##
#W  examples.gd           Examples [loops]
##  
##  
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  DISPLAYING INFORMATION ABOUT A LIBRARY
##  -------------------------------------------------------------------------
DeclareGlobalFunction( "DisplayLibraryInfo" );

#############################################################################
##  READING LOOP FROM THE LIBRARY - GENERIC METHOD
##  -------------------------------------------------------------------------
DeclareGlobalFunction( "LibraryLoop" );

# up to isomorphism

DeclareGlobalFunction( "LeftBolLoop" );
DeclareGlobalFunction( "RightBolLoop" );
DeclareGlobalFunction( "MoufangLoop" );
DeclareGlobalFunction( "PaigeLoop" );
DeclareGlobalFunction( "CodeLoop" );
DeclareGlobalFunction( "SteinerLoop" );
DeclareGlobalFunction( "CCLoop" );
DeclareGlobalFunction( "ConjugacyClosedLoop" );
DeclareGlobalFunction( "RCCLoop" );
DeclareGlobalFunction( "RightConjugacyClosedLoop" );
DeclareGlobalFunction( "LCCLoop" );
DeclareGlobalFunction( "LeftConjugacyClosedLoop" );
DeclareGlobalFunction( "SmallLoop" );
DeclareGlobalFunction( "InterestingLoop" );
DeclareGlobalFunction( "NilpotentLoop" );
DeclareGlobalFunction( "AutomorphicLoop" );
DeclareGlobalFunction( "LeftBruckLoop" );
DeclareGlobalFunction( "RightBruckLoop" );

# up to isotopism

DeclareGlobalFunction( "ItpSmallLoop" );

# auxiliary
DeclareGlobalFunction( "LOOPS_LibraryByName" );
DeclareGlobalFunction( "LOOPS_SmallestNonsquare" );
DeclareGlobalFunction( "LOOPS_ActivateLeftBolLoopPQ" );
DeclareGlobalFunction( "LOOPS_ActivateLeftBolLoop" );
DeclareGlobalFunction( "LOOPS_ActivateMoufangLoop" );
DeclareGlobalFunction( "LOOPS_ActivateSteinerLoop" );
DeclareGlobalFunction( "LOOPS_ActivateRCCLoop" );
DeclareGlobalFunction( "LOOPS_ActivateCCLoop" );
DeclareGlobalFunction( "LOOPS_ActivateNilpotentLoop" );
DeclareGlobalFunction( "LOOPS_ActivateAutomorphicLoop" );
DeclareGlobalFunction( "LOOPS_ActivateRightBruckLoop" );
