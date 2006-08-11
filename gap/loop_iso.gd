############################################################################
##
#W  loop_iso.gd      Isomorphisms of loops      G. P. Nagy / P. Vojtechovsky
##  
#H  @(#)$Id: loop_iso.gd, v 1.2.1 2006/8/11 gap Exp $
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

#############################################################################
##  AUTOMORPHISMS AND AUTOMORPHISM GROUPS
##  -------------------------------------------------------------------------

# AutomorphismGroup already declared for groups

#############################################################################
##  ISOMORPHISM TYPE OF MOUFANG LOOP
##  ------------------------------------------------------------------------
DeclareOperation( "IsomorphismTypeOfMoufangLoop", [ IsLoop ] );

# There are several function in this package that we do not wish to make 
# global, namely:
# IsomorphismLoopsNC
# EfficientGenerators
# ExtendHomomorphismByClosingSource
# SublistPosition
# ExtendIsomorphism
# AutomorphismsFixingSet
