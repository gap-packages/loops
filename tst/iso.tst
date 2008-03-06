#############################################################################
##
#W  iso.tst   Testing isomorphisms             G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: iso.tst, v 2.0.0 2008/03/06 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

gap> START_TEST("LOOPS, iso: testing isomorphisms");

# TESTING AUTOMORPHISM GROUPS

gap> AutomorphismGroup( MoufangLoop( 12, 1 ) );
<permutation group with 14 generators>
gap> AutomorphismGroup( MoufangLoop( 64, 1235 ) );
<permutation group with 16 generators>
gap> Size( last );
512
gap> AutomorphismGroup( LeftBolLoop( 8, 1 ) );
Group([ (5,8)(6,7), (2,3)(6,7), (2,6)(3,7), (2,7)(3,6) ])
gap> Size( AutomorphismGroup( SteinerLoop( 16, 77 ) ) );
3

# TESTING ISOMORPHISMS

gap> Q := DirectProduct( MoufangLoop( 32, 5 ) );;
gap> Qp := IsomorphicCopyByPerm( Q, (2,3,4)(17,20) );;
gap> IsomorphismLoops( Q, Qp );
(2,3,4)(18,23)(19,25)(21,27)(22,28)(24,30)(26,31)(29,32)

# TESTING ISOTOPISMS

gap> IsotopismLoops( SmallLoop( 5, 1 ), SmallLoop( 5, 4 ) );    
[ (3,5,4), (1,2), (1,2) ]

gap> STOP_TEST( "iso.tst", 10000000 );
