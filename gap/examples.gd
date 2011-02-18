#############################################################################
##
#W  examples.gd           Examples [loops]
##  
#H  @(#)$Id: examples.gd, v 2.0.1 2008/03/12 gap Exp $
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
DeclareGlobalFunction( "MoufangLoop" );
DeclareGlobalFunction( "PaigeLoop" );
DeclareGlobalFunction( "CodeLoop" );
DeclareGlobalFunction( "SteinerLoop" );
DeclareGlobalFunction( "CCLoop" );
DeclareGlobalFunction( "SmallLoop" );
DeclareGlobalFunction( "InterestingLoop" );
DeclareGlobalFunction( "NilpotentLoop" );

# up to isotopism

DeclareGlobalFunction( "ItpSmallLoop" );
