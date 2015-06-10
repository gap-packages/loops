#############################################################################
##
#W  iso.gd  Isomorphisms and isotopisms [loops]
##  
#H  @(#)$Id: iso.gd, v 3.0.0 2015/06/12 gap Exp $
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


#############################################################################
##  ISOTOPISM OF LOOPS
##  ------------------------------------------------------------------------
DeclareOperation( "IsotopismLoops", [ IsLoop, IsLoop ] );
DeclareOperation( "LoopsUpToIsotopism", [ IsList ] );


#############################################################################
##  AUXILIARY
##  -------------------------------------------------------------------------
DeclareGlobalFunction( "IsomorphismLoopsNC" );
DeclareGlobalFunction( "LOOPS_EfficientGenerators" );
DeclareGlobalFunction( "LOOPS_ExtendHomomorphismByClosingSource" );
DeclareGlobalFunction( "LOOPS_ExtendIsomorphism" );
DeclareGlobalFunction( "LOOPS_SublistPosition" );
DeclareGlobalFunction( "LOOPS_AutomorphismsFixingSet" );
