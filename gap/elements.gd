#############################################################################
##
#W  elements.gd  Elements and basic arithmetic operations [loops]
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  DIVISION
##  -------------------------------------------------------------------------

DeclareOperation( "LeftDivision",
    [ IsQuasigroupElement, IsQuasigroupElement ] );
DeclareOperation( "RightDivision",
    [ IsQuasigroupElement, IsQuasigroupElement ] );
DeclareOperation( "LeftDivisionCayleyTable", [ IsQuasigroup ] );
DeclareOperation( "RightDivisionCayleyTable", [ IsQuasigroup ] );

#############################################################################
##  POWERS AND INVERSES
##  -------------------------------------------------------------------------

DeclareAttribute( "One", IsLoopElement );
DeclareAttribute( "RightInverse", IsLoopElement );
DeclareAttribute( "LeftInverse", IsLoopElement );

#############################################################################
##  ASSOCIATORS AND COMMUTATORS
##  -------------------------------------------------------------------------

DeclareOperation( "Associator",
    [ IsQuasigroupElement, IsQuasigroupElement, IsQuasigroupElement ] );
DeclareOperation( "Commutator",
    [ IsQuasigroupElement, IsQuasigroupElement ] );
