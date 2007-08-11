#############################################################################
##
#W  examples.gd           Examples of loops      G. P. Nagy / P. Vojtechovsky
##  
#H  @(#)$Id: examples.gd, v 1.5.1 2007/06/27 gap Exp $
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

# up to isotopism

DeclareGlobalFunction( "ItpSmallLoop" );
