#############################################################################
##
#W  quasigrp.gd      Basic methods for q & l     G. P. Nagy / P. Vojtechovsky
##  
#H  @(#)$Id: quasigrp.gd, v 1.9.0 2007/08/06 gap Exp $
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
DeclareCategory( "IsLatin", IsObject );

## quasigroup
DeclareCategory( "IsQuasigroup", IsMagma and IsLatin );
    
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

DeclareOperation( "QuasigroupByCayleyTable", [ IsMatrix ] );
DeclareOperation( "LoopByCayleyTable", [ IsMatrix ] );
DeclareOperation( "SetQuasigroupElmName", [ IsQuasigroup, IsString ] );
DeclareSynonym( "SetLoopElmName", SetQuasigroupElmName );

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
# There are also versions of QuasigroupByRightSection and LoopByRightSection 
# for [ IsGroup, IsGroup, IsMultiplicativeElementCollection ]

#############################################################################
##  CONVERSIONS
##  -------------------------------------------------------------------------

DeclareOperation( "AsQuasigroup", [ IsMagma ] );
DeclareOperation( "PrincipalLoopIsotope", 
    [ IsQuasigroup, IsQuasigroupElement, IsQuasigroupElement ] );
DeclareOperation( "AsLoop", [ IsMagma ] );
# AsGroup already declared as attribute of IsList

#############################################################################
## PRODUCTS OF LOOPS
## --------------------------------------------------------------------------

#DirectProduct already declared for groups.

#############################################################################
## OPPOSITE QUASIGROUPS AND LOOPS
## --------------------------------------------------------------------------

DeclareOperation( "Opposite", [ IsQuasigroup ] );

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

#############################################################################
##  BASIC ATTRIBUTES
##  -------------------------------------------------------------------------

DeclareAttribute( "CayleyTable", IsQuasigroup );
DeclareAttribute( "Exponent", IsLoop );

#############################################################################
##  GENERATORS
##  -------------------------------------------------------------------------
 
DeclareSynonymAttr( "GeneratorsOfQuasigroup", GeneratorsOfMagma );
DeclareSynonymAttr( "GeneratorsOfLoop", GeneratorsOfQuasigroup );
# attribute GeneratorsSmallest already declared for groups

#############################################################################
##  SECTIONS
##  -------------------------------------------------------------------------
 
DeclareAttribute( "LeftSection", IsQuasigroup );
DeclareAttribute( "RightSection", IsQuasigroup );
DeclareOperation( "LeftTranslation", 
    [ IsQuasigroup, IsQuasigroupElement ] );
DeclareOperation( "RightTranslation",
    [ IsQuasigroup, IsQuasigroupElement ] );

#############################################################################
##  MULTIPLICATION GROUPS
##  -------------------------------------------------------------------------
 
DeclareAttribute( "LeftMultiplicationGroup", IsQuasigroup );
DeclareAttribute( "RightMultiplicationGroup", IsQuasigroup );
DeclareAttribute( "MultiplicationGroup", IsQuasigroup );
DeclareOperation( "RelativeRightMultiplicationGroup", 
    [ IsQuasigroup, IsQuasigroup ] );
DeclareOperation( "RelativeLeftMultiplicationGroup",
    [ IsQuasigroup, IsQuasigroup ] );
DeclareOperation( "RelativeMultiplicationGroup",
    [ IsQuasigroup, IsQuasigroup ] );

#############################################################################
##  INNER MAPPING GROUPS
##  -------------------------------------------------------------------------

DeclareOperation( "LeftInnerMapping", [ IsLoop, IsLoopElement, IsLoopElement ] );
DeclareOperation( "RightInnerMapping", [ IsLoop, IsLoopElement, IsLoopElement ] );
DeclareOperation( "MiddleInnerMapping", [ IsLoop, IsLoopElement ] );
DeclareAttribute( "InnerMappingGroup", IsLoop );
DeclareAttribute( "LeftInnerMappingGroup", IsLoop );
DeclareAttribute( "MiddleInnerMappingGroup", IsLoop );
DeclareAttribute( "RightInnerMappingGroup", IsLoop );

#############################################################################
##  SUBQUASIGROUPS AND SUBLOOPS
##  -------------------------------------------------------------------------

DeclareOperation( "PosInParent", [ IsCollection ] ); 
# Position already declared in GAP
DeclareOperation( "SubquasigroupNC", [ IsQuasigroup, IsSet ] );
DeclareOperation( "Subquasigroup", [ IsQuasigroup, IsList ] ); 
DeclareOperation( "Subloop", [ IsLoop, IsList ] );
DeclareOperation( "IsSubquasigroup", [ IsQuasigroup, IsQuasigroup ] );
DeclareOperation( "IsSubloop", [ IsLoop, IsLoop ] );
DeclareOperation( "AllSubloops", [ IsLoop ] );
# RightCosets already declared in GAP
DeclareOperation( "RightTransversal", [ IsLoop, IsLoop ] );

#############################################################################
##  NUCLEUS, COMMUTANT, CENTER
##  -------------------------------------------------------------------------

DeclareAttribute( "LeftNucleus", IsQuasigroup );
DeclareAttribute( "RightNucleus", IsQuasigroup );
DeclareAttribute( "MiddleNucleus", IsQuasigroup );
DeclareAttribute( "Nuc", IsQuasigroup );
DeclareSynonymAttr( "NucleusOfLoop", Nuc );
DeclareSynonymAttr( "NucleusOfQuasigroup", Nuc );
DeclareAttribute( "Commutant", IsQuasigroup );
DeclareAttribute( "Center", IsQuasigroup );
DeclareAttribute( "AssociatorSubloop", IsLoop );

#############################################################################
##  ASSOCIATIVITY, COMMUTATIVITY AND GENERALIZATIONS
##  -------------------------------------------------------------------------

DeclareProperty( "IsAssociative", IsQuasigroup );
DeclareProperty( "IsCommutative", IsQuasigroup );
DeclareProperty( "IsPowerAssociative", IsLoop );
DeclareProperty( "IsDiassociative", IsLoop );

#############################################################################
##  INVERSE PROPERTIES
##  -------------------------------------------------------------------------

DeclareProperty( "HasLeftInverseProperty", IsLoop );
DeclareProperty( "HasRightInverseProperty", IsLoop );
DeclareProperty( "HasInverseProperty", IsLoop );
DeclareProperty( "HasWeakInverseProperty", IsLoop );
DeclareProperty( "HasTwosidedInverses", IsLoop );
DeclareProperty( "HasAutomorphicInverseProperty", IsLoop );
DeclareProperty( "HasAntiautomorphicInverseProperty", IsLoop );

#############################################################################
##  PROPERTIES OF QUASIGROUPS
##  -------------------------------------------------------------------------

DeclareProperty( "IsSemisymmetric", IsQuasigroup );
DeclareProperty( "IsTotallySymmetric", IsQuasigroup );
DeclareProperty( "IsIdempotent", IsQuasigroup );
DeclareProperty( "IsSteinerQuasigroup", IsQuasigroup );
DeclareProperty( "IsUnipotent", IsQuasigroup );
DeclareProperty( "IsLDistributive", IsQuasigroup );
DeclareProperty( "IsRDistributive", IsQuasigroup );
DeclareSynonymAttr( "IsLeftDistributive", IsLDistributive );
DeclareSynonymAttr( "IsRightDistributive", IsRDistributive );
#IsDistributive already declared in GAP
DeclareProperty( "IsEntropic", IsQuasigroup );
DeclareSynonymAttr( "IsMedial", IsEntropic );

#############################################################################
##  LOOPS OF BOL-MOUFANG TYPE
##  -------------------------------------------------------------------------

DeclareProperty( "IsExtraLoop", IsLoop );
DeclareProperty( "IsMoufangLoop", IsLoop );
DeclareProperty( "IsCLoop", IsLoop );
DeclareProperty( "IsLeftBolLoop", IsLoop );
DeclareProperty( "IsRightBolLoop", IsLoop );
DeclareProperty( "IsLCLoop", IsLoop );
DeclareProperty( "IsRCLoop", IsLoop );
DeclareProperty( "IsLeftNuclearSquareLoop", IsLoop );
DeclareProperty( "IsMiddleNuclearSquareLoop", IsLoop );
DeclareProperty( "IsRightNuclearSquareLoop", IsLoop );
DeclareProperty( "IsNuclearSquareLoop", IsLoop );
DeclareProperty( "IsFlexible", IsQuasigroup );
DeclareProperty( "IsLeftAlternative", IsQuasigroup );
DeclareProperty( "IsRightAlternative", IsQuasigroup );
DeclareProperty( "IsAlternative", IsQuasigroup );

#############################################################################
##  POWER ALTERNATIVE LOOPS
##  -------------------------------------------------------------------------

DeclareProperty( "IsLeftPowerAlternative", IsLoop );
DeclareProperty( "IsRightPowerAlternative", IsLoop );
DeclareProperty( "IsPowerAlternative", IsLoop );

#############################################################################
##  CC-LOOPS AND RELATED PROPERTIES
##  -------------------------------------------------------------------------

DeclareProperty( "IsLCCLoop", IsLoop );
DeclareProperty( "IsRCCLoop", IsLoop );
DeclareProperty( "IsCCLoop", IsLoop );
DeclareProperty( "IsOsbornLoop", IsLoop );

############################################################################
##  ADDITIONAL VARIETIES OF LOOPS
##  -------------------------------------------------------------------------

DeclareProperty( "IsCodeLoop", IsLoop );
DeclareProperty( "IsSteinerLoop", IsLoop );
DeclareProperty( "IsLeftBruckLoop", IsLoop );
DeclareSynonymAttr( "IsLeftKLoop", IsLeftBruckLoop );
DeclareProperty( "IsRightBruckLoop", IsLoop );
DeclareSynonymAttr( "IsRightKLoop", IsRightBruckLoop );
DeclareProperty( "IsALoop", IsLoop );
DeclareProperty( "IsLeftALoop", IsLoop );
DeclareProperty( "IsMiddleALoop", IsLoop );
DeclareProperty( "IsRightALoop", IsLoop );

#############################################################################
##  NORMALITY
##  -------------------------------------------------------------------------

InParentFOA( "IsNormal", IsLoop, IsLoop, DeclareProperty );
InParentFOA( "NormalClosure", IsLoop, IsLoop, DeclareAttribute );
# IsSimple is already declared for groups

#############################################################################
##  FACTOR LOOP
##  -------------------------------------------------------------------------

DeclareOperation( "FactorLoop", [ IsLoop, IsLoop ] );
DeclareOperation( "NaturalHomomorphismByNormalSubloop", [ IsLoop, IsLoop ] );

#############################################################################
##  NILPOTENCY
##  -------------------------------------------------------------------------

DeclareAttribute( "NilpotencyClassOfLoop", IsLoop );
# IsNilpotent is already declared for groups
DeclareAttribute( "IsStronglyNilpotent", IsLoop );
# UpperCentralSeries already declared for groups.
# LowerCentralSeries already declared for groups.

#############################################################################
##  SOLVABILITY
##  -------------------------------------------------------------------------

# IsSolvable is already declared for groups
DeclareAttribute( "DerivedSubloop", IsLoop );
# DerivedLength already declared for groups
DeclareAttribute( "FrattiniSubloop", IsLoop );
# FrattinifactorSize already declared for groups
