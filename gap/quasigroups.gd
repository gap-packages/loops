#############################################################################
##
#W  quasigroups.gd  Representing, creating and displaying quasigroups [loops]
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  GAP CATEGORIES AND REPRESENTATIONS
##  -------------------------------------------------------------------------

## element of a quasigroup
DeclareCategory( "IsQuasigroupElement", IsMultiplicativeElement );
DeclareRepresentation( "IsQuasigroupElmRep",
    IsPositionalObjectRep and IsMultiplicativeElement, [1] );

## element of a loop
DeclareCategory( "IsLoopElement",
    IsQuasigroupElement and IsMultiplicativeElementWithInverse );
DeclareRepresentation( "IsLoopElmRep",
    IsPositionalObjectRep and IsMultiplicativeElementWithInverse, [1] );

## latin (auxiliary category for GAP to tell apart IsMagma and IsQuasigroup)
DeclareCategory( "IsLatinMagma", IsObject );

## quasigroup
DeclareCategory( "IsQuasigroup", IsMagma and IsLatinMagma );

## loop
DeclareCategory( "IsLoop", IsQuasigroup and IsMultiplicativeElementWithInverseCollection);

#############################################################################
##  TESTING MULTIPLICATION TABLES
##  -------------------------------------------------------------------------

DeclareOperation( "IsQuasigroupTable", [ IsMatrix ] );
DeclareSynonym( "IsQuasigroupCayleyTable", IsQuasigroupTable );
DeclareOperation( "IsLoopTable", [ IsMatrix ] );
DeclareSynonym( "IsLoopCayleyTable", IsLoopTable );
DeclareOperation( "CanonicalCayleyTable", [ IsMatrix ] );
DeclareOperation( "NormalizedQuasigroupTable", [ IsMatrix ] );

#############################################################################
##  CREATING QUASIGROUPS AND LOOPS MANUALLY
##  -------------------------------------------------------------------------

DeclareAttribute( "CayleyTable", IsQuasigroup );
DeclareOperation( "QuasigroupByCayleyTable", [ IsMatrix ] );
DeclareOperation( "LoopByCayleyTable", [ IsMatrix ] );
DeclareOperation( "SetQuasigroupElmName", [ IsQuasigroup, IsString ] );
DeclareSynonym( "SetLoopElmName", SetQuasigroupElmName );
DeclareOperation( "CanonicalCopy", [ IsQuasigroup ] );

#############################################################################
##  CREATING QUASIGROUPS AND LOOPS FROM A FILE
##  -------------------------------------------------------------------------

DeclareOperation( "QuasigroupFromFile", [ IsString, IsString ] );
DeclareOperation( "LoopFromFile", [ IsString, IsString ] );

#############################################################################
##  CREATING QUASIGROUPS AND LOOPS BY SECTIONS
##  -------------------------------------------------------------------------

DeclareOperation( "CayleyTableByPerms", [ IsPermCollection ] );
DeclareOperation( "QuasigroupByLeftSection", [ IsPermCollection ] );
DeclareOperation( "LoopByLeftSection", [ IsPermCollection ] );
DeclareOperation( "QuasigroupByRightSection", [ IsPermCollection ] );
DeclareOperation( "LoopByRightSection", [ IsPermCollection ] );
DeclareOperation( "QuasigroupByRightFolder", [ IsGroup, IsGroup, IsMultiplicativeElementCollection ] );
DeclareOperation( "LoopByRightFolder", [ IsGroup, IsGroup, IsMultiplicativeElementCollection ] );

#############################################################################
##  CONVERSIONS
##  -------------------------------------------------------------------------

DeclareOperation( "IntoQuasigroup", [ IsMagma ] );
DeclareOperation( "PrincipalLoopIsotope",
    [ IsQuasigroup, IsQuasigroupElement, IsQuasigroupElement ] );
DeclareOperation( "IntoLoop", [ IsMagma ] );
DeclareOperation( "IntoGroup", [ IsMagma ] );

#############################################################################
## PRODUCTS OF QUASIGROUPS AND LOOPS
## --------------------------------------------------------------------------

#DirectProduct already declared for groups.

#############################################################################
## OPPOSITE QUASIGROUPS AND LOOPS
## --------------------------------------------------------------------------

DeclareOperation( "OppositeQuasigroup", [ IsQuasigroup ] );
DeclareOperation( "OppositeLoop", [ IsLoop ] );
DeclareAttribute( "Opposite", IsQuasigroup );

#############################################################################
## AUXILIARY
## --------------------------------------------------------------------------
DeclareGlobalFunction( "LOOPS_ReadCayleyTableFromFile" );
DeclareGlobalFunction( "LOOPS_CayleyTableByRightFolder" );
