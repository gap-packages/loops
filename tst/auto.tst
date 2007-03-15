#############################################################################
##
#W  auto.tst   Testing automorphisms             G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: auto.tst, v 1.5.0 2007/03/06 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

gap> START_TEST("LOOPS, auto: testing automorphism groups");

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

gap> DirectProduct( MoufangLoop( 32, 5 ), Group( (1,2) ) );;
gap> IsomorphismTypeOfMoufangLoop( last );
[ [ 64, 19 ], (6,7,8,9,11,14,20,36,15,23,40,22,51,35,13,18,34,10,12,17,33)(16,
    27,46,30,52,47,31,63,55,53,48,32,64,59,58,44,28,60,49,38,19,45,29,61,50,
    42,25,56,54,39,21,37)(24,41)(26,57,43) ]

# TESTING ISOTOPISMS

gap> IsotopismLoops( SmallLoop( 5, 1 ), SmallLoop( 5, 4 ) );    
[ (3,5,4), (1,2), (1,2) ]

gap> STOP_TEST( "auto.tst", 10000000 );
