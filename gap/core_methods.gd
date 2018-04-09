#############################################################################
##
#W  core_methods.gd  Most common structural methods [loops]
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  GENERATORS
##  -------------------------------------------------------------------------

DeclareSynonymAttr( "GeneratorsOfQuasigroup", GeneratorsOfMagma );
DeclareSynonymAttr( "GeneratorsOfLoop", GeneratorsOfQuasigroup );
# attribute GeneratorsSmallest already declared for groups
# attribite SmallGeneratingSet already declared for groups

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
DeclareOperation( "AllSubquasigroups", [ IsQuasigroup ] );
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
##  EXPONENT
##  -------------------------------------------------------------------------

DeclareAttribute( "Exponent", IsLoop );

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
