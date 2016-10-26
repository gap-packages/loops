#############################################################################
##
#W  classes.gd  Testing varieties [loops]
##
#H  @(#)$Id: classes.gd, v 3.3.0 2016/10/26 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  ASSOCIATIVITY, COMMUTATIVITY AND GENERALIZATIONS
##  -------------------------------------------------------------------------

DeclareProperty( "IsAssociative", IsLoop );
DeclareProperty( "IsCommutative", IsQuasigroup );
DeclareProperty( "IsPowerAssociative", IsQuasigroup );
DeclareProperty( "IsDiassociative", IsQuasigroup );

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
DeclareSynonymAttr( "IsLeftConjugacyClosedLoop", IsLCCLoop );
DeclareProperty( "IsRCCLoop", IsLoop );
DeclareSynonymAttr( "IsRightConjugacyClosedLoop", IsRCCLoop );
DeclareProperty( "IsCCLoop", IsLoop );
DeclareSynonymAttr( "IsConjugacyClosedLoop", IsCCLoop );
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
DeclareSynonymAttr( "IsAutomorphicLoop", IsALoop );
DeclareProperty( "IsLeftALoop", IsLoop );
DeclareSynonymAttr( "IsLeftAutomorphicLoop", IsLeftALoop );
DeclareProperty( "IsMiddleALoop", IsLoop );
DeclareSynonymAttr( "IsMiddleAutomorphicLoop", IsMiddleALoop );
DeclareProperty( "IsRightALoop", IsLoop );
DeclareSynonymAttr( "IsRightAutomorphicLoop", IsRightALoop );
