#############################################################################
##
#W  bol_core_methods.gd     Common methods for Bol loops [loops]
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

DeclareAttribute( "AssociatedLeftBruckLoop", IsLeftBolLoop );
DeclareAttribute( "AssociatedRightBruckLoop", IsRightBolLoop );

DeclareOperation( "IsExactGroupFactorization", [ IsGroup, IsGroup, IsGroup ] );
DeclareGlobalFunction( "RightBolLoopByExactGroupFactorizationNC" ); # auxiliary
DeclareGlobalFunction( "RightBolLoopByExactGroupFactorization" ); # variable arguments


