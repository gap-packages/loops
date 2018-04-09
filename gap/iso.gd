#############################################################################
##
#W  iso.gd  Isomorphisms and isotopisms [loops]
##  
##  
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##


#############################################################################
##  DISCRIMINATOR
##  -------------------------------------------------------------------------
DeclareOperation( "Discriminator", [ IsQuasigroup ] );
DeclareOperation( "AreEqualDiscriminators", [ IsList, IsList ] );


#############################################################################
##  ISOMORPHISMS OF QUASIGROUPS AND LOOPS
##  -------------------------------------------------------------------------
DeclareOperation( "IsomorphismQuasigroups", [ IsQuasigroup, IsQuasigroup ] );
DeclareOperation( "IsomorphismLoops", [ IsLoop, IsLoop ] );
DeclareOperation( "QuasigroupsUpToIsomorphism", [ IsList ] );
DeclareOperation( "LoopsUpToIsomorphism", [ IsList ] );
DeclareOperation( "QuasigroupIsomorph", [ IsQuasigroup, IsPerm ] );
DeclareOperation( "LoopIsomorph", [ IsLoop, IsPerm ] );
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
DeclareGlobalFunction( "IsomorphismQuasigroupsNC" );
DeclareGlobalFunction( "LOOPS_EfficientGenerators" );
DeclareGlobalFunction( "LOOPS_ExtendHomomorphismByClosingSource" );
DeclareGlobalFunction( "LOOPS_ExtendIsomorphism" );
DeclareGlobalFunction( "LOOPS_SublistPosition" );
DeclareGlobalFunction( "LOOPS_AutomorphismsFixingSet" );
