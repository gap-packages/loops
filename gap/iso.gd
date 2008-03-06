#############################################################################
##
#W  iso.gd  Isomorphisms and isotopisms [loops]
##  
#H  @(#)$Id: iso.gd, v 2.0.0 2008/03/07 gap Exp $
##  
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  DISCRIMINATOR
##  -------------------------------------------------------------------------
DeclareOperation( "Discriminator", [ IsLoop ] );
DeclareOperation( "AreEqualDiscriminators", [ IsList, IsList ] );

#############################################################################
##  ISOMORPHISMS OF LOOPS
##  -------------------------------------------------------------------------
DeclareOperation( "IsomorphismLoops", [ IsLoop, IsLoop ] );
DeclareOperation( "LoopsUpToIsomorphism", [ IsList ] );
DeclareOperation( "IsomorphicCopyByPerm", [ IsQuasigroup, IsPerm ] );
DeclareOperation( "IsomorphicCopyByNormalSubloop", [ IsLoop, IsLoop ] );

#############################################################################
##  AUTOMORPHISMS AND AUTOMORPHISM GROUPS
##  -------------------------------------------------------------------------

# AutomorphismGroup already declared for groups

# There are several function in this package that we do not wish to make 
# global, namely:
# IsomorphismLoopsNC
# EfficientGenerators
# ExtendHomomorphismByClosingSource
# SublistPosition
# ExtendIsomorphism
# AutomorphismsFixingSet

#############################################################################
##  ISOTOPISM OF LOOPS
##  ------------------------------------------------------------------------
DeclareOperation( "IsotopismLoops", [ IsLoop, IsLoop ] );
DeclareOperation( "LoopsUpToIsotopism", [ IsList ] );
