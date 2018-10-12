#############################################################################
##
#W  classes.gd  Testing varieties [loops]
##
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

############################################################################
##  IMPLICATIONS for InstallTrueMethod
##  -------------------------------------------------------------------------

# implies
InstallTrueMethod( IsExtraLoop, IsAssociative and IsLoop );
InstallTrueMethod( HasTwosidedInverses, IsPowerAssociative and IsLoop );
InstallTrueMethod( IsPowerAlternative, IsDiassociative );
InstallTrueMethod( IsFlexible, IsDiassociative );
InstallTrueMethod( IsMoufangLoop, IsExtraLoop );
InstallTrueMethod( IsCLoop, IsExtraLoop );
InstallTrueMethod( IsLeftBolLoop, IsMoufangLoop );
InstallTrueMethod( IsRightBolLoop, IsMoufangLoop );
InstallTrueMethod( IsDiassociative, IsMoufangLoop );
InstallTrueMethod( IsLCLoop, IsCLoop );
InstallTrueMethod( IsRCLoop, IsCLoop );
InstallTrueMethod( IsDiassociative, IsCLoop and IsFlexible);
InstallTrueMethod( IsRightBolLoop, IsLeftBolLoop and IsCommutative );
InstallTrueMethod( IsLeftPowerAlternative, IsLeftBolLoop );
InstallTrueMethod( IsLeftBolLoop, IsRightBolLoop and IsCommutative );
InstallTrueMethod( IsRightPowerAlternative, IsRightBolLoop );
InstallTrueMethod( IsLeftPowerAlternative, IsLCLoop );
InstallTrueMethod( IsLeftNuclearSquareLoop, IsLCLoop );
InstallTrueMethod( IsMiddleNuclearSquareLoop, IsLCLoop );
InstallTrueMethod( IsRCLoop, IsLCLoop and IsCommutative );
InstallTrueMethod( IsRightPowerAlternative, IsRCLoop );
InstallTrueMethod( IsRightNuclearSquareLoop, IsRCLoop );
InstallTrueMethod( IsMiddleNuclearSquareLoop, IsRCLoop );
InstallTrueMethod( IsLCLoop, IsRCLoop and IsCommutative );
InstallTrueMethod( IsRightNuclearSquareLoop, IsLeftNuclearSquareLoop and IsCommutative );
InstallTrueMethod( IsLeftNuclearSquareLoop, IsRightNuclearSquareLoop and IsCommutative );
InstallTrueMethod( IsLeftNuclearSquareLoop, IsNuclearSquareLoop );
InstallTrueMethod( IsRightNuclearSquareLoop, IsNuclearSquareLoop );
InstallTrueMethod( IsMiddleNuclearSquareLoop, IsNuclearSquareLoop );
InstallTrueMethod( IsRightAlternative, IsLeftAlternative and IsCommutative );
InstallTrueMethod( IsLeftAlternative, IsRightAlternative and IsCommutative );
InstallTrueMethod( IsLeftAlternative, IsAlternative );
InstallTrueMethod( IsRightAlternative, IsAlternative );
InstallTrueMethod( IsLeftAlternative, IsLeftPowerAlternative );
InstallTrueMethod( HasLeftInverseProperty, IsLeftPowerAlternative );
InstallTrueMethod( IsPowerAssociative, IsLeftPowerAlternative );
InstallTrueMethod( IsRightAlternative, IsRightPowerAlternative );
InstallTrueMethod( HasRightInverseProperty, IsRightPowerAlternative );
InstallTrueMethod( IsPowerAssociative, IsRightPowerAlternative );
InstallTrueMethod( IsLeftPowerAlternative, IsPowerAlternative );
InstallTrueMethod( IsRightPowerAlternative, IsPowerAlternative );
InstallTrueMethod( IsAssociative, IsLCCLoop and IsCommutative );
InstallTrueMethod( IsExtraLoop, IsLCCLoop and IsMoufangLoop );
InstallTrueMethod( IsAssociative, IsRCCLoop and IsCommutative );
InstallTrueMethod( IsExtraLoop, IsRCCLoop and IsMoufangLoop );
InstallTrueMethod( IsLCCLoop, IsCCLoop );
InstallTrueMethod( IsRCCLoop, IsCCLoop );
InstallTrueMethod( IsExtraLoop, IsCodeLoop );
InstallTrueMethod( IsCCLoop, IsCodeLoop );
InstallTrueMethod( IsCommutative, IsSteinerLoop );
InstallTrueMethod( IsCLoop, IsSteinerLoop );
InstallTrueMethod( HasAutomorphicInverseProperty, IsLeftBruckLoop );
InstallTrueMethod( IsLeftBolLoop, IsLeftBruckLoop );
InstallTrueMethod( IsRightBruckLoop, IsLeftBruckLoop and IsCommutative );
InstallTrueMethod( HasAutomorphicInverseProperty, IsRightBruckLoop );
InstallTrueMethod( IsRightBolLoop, IsRightBruckLoop );
InstallTrueMethod( IsLeftBruckLoop, IsRightBruckLoop and IsCommutative );
InstallTrueMethod( IsLeftALoop, IsALoop );
InstallTrueMethod( IsRightALoop, IsALoop );
InstallTrueMethod( IsMiddleALoop, IsALoop );
InstallTrueMethod( IsLeftALoop, IsRightALoop and HasAntiautomorphicInverseProperty );
InstallTrueMethod( IsRightALoop, IsLeftALoop and HasAntiautomorphicInverseProperty );
InstallTrueMethod( IsFlexible, IsMiddleALoop );
InstallTrueMethod( HasAntiautomorphicInverseProperty, IsFlexible and IsLeftALoop );
InstallTrueMethod( HasAntiautomorphicInverseProperty, IsFlexible and IsRightALoop );
InstallTrueMethod( IsMoufangLoop, IsALoop and IsLeftAlternative );
InstallTrueMethod( IsMoufangLoop, IsALoop and IsRightAlternative );
InstallTrueMethod( IsMoufangLoop, IsALoop and HasLeftInverseProperty );
InstallTrueMethod( IsMoufangLoop, IsALoop and HasRightInverseProperty );
InstallTrueMethod( IsMoufangLoop, IsALoop and HasWeakInverseProperty );

# is implied by
InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsLeftNuclearSquareLoop );
InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsMiddleNuclearSquareLoop );
InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsRightNuclearSquareLoop );
InstallTrueMethod( IsMoufangLoop, IsLeftBolLoop and IsRightBolLoop );
InstallTrueMethod( IsCLoop, IsLCLoop and IsRCLoop );
InstallTrueMethod( IsNuclearSquareLoop, IsLeftNuclearSquareLoop
        and IsRightNuclearSquareLoop and IsMiddleNuclearSquareLoop );
InstallTrueMethod( IsFlexible, IsCommutative );
InstallTrueMethod( IsAlternative, IsLeftAlternative and IsRightAlternative );
InstallTrueMethod( IsCCLoop, IsLCCLoop and IsRCCLoop );
InstallTrueMethod( IsOsbornLoop, IsMoufangLoop );
InstallTrueMethod( IsOsbornLoop, IsCCLoop );
InstallTrueMethod( IsLeftBruckLoop, IsLeftBolLoop and HasAutomorphicInverseProperty );
InstallTrueMethod( IsRightBruckLoop, IsRightBolLoop and HasAutomorphicInverseProperty );
InstallTrueMethod( IsMiddleALoop, IsCommutative and IsLoop);
InstallTrueMethod( IsLeftALoop, IsLeftBruckLoop );
InstallTrueMethod( IsLeftALoop, IsLCCLoop );
InstallTrueMethod( IsRightALoop, IsRightBruckLoop );
InstallTrueMethod( IsRightALoop, IsRCCLoop );
InstallTrueMethod( IsALoop, IsCommutative and IsMoufangLoop );
InstallTrueMethod( IsALoop, IsLeftALoop and IsMiddleALoop );
InstallTrueMethod( IsALoop, IsRightALoop and IsMiddleALoop );
InstallTrueMethod( IsALoop, IsAssociative and IsLoop);

# implies and is implied by (for inverse properties)
InstallTrueMethod( HasAntiautomorphicInverseProperty, HasAutomorphicInverseProperty and IsCommutative );
InstallTrueMethod( HasAutomorphicInverseProperty, HasAntiautomorphicInverseProperty and IsCommutative );
InstallTrueMethod( HasLeftInverseProperty, HasInverseProperty );
InstallTrueMethod( HasRightInverseProperty, HasInverseProperty );
InstallTrueMethod( HasWeakInverseProperty, HasInverseProperty );
InstallTrueMethod( HasAntiautomorphicInverseProperty, HasInverseProperty );
InstallTrueMethod( HasTwosidedInverses, HasAntiautomorphicInverseProperty );
InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and IsCommutative );
InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and IsCommutative );
InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasRightInverseProperty );
InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasWeakInverseProperty );
InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and HasWeakInverseProperty );
InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasAntiautomorphicInverseProperty );
InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and HasAntiautomorphicInverseProperty );
InstallTrueMethod( HasInverseProperty, HasWeakInverseProperty and HasAntiautomorphicInverseProperty );
InstallTrueMethod( HasTwosidedInverses, HasLeftInverseProperty );
InstallTrueMethod( HasTwosidedInverses, HasRightInverseProperty );
InstallTrueMethod( HasTwosidedInverses, IsFlexible and IsLoop );
