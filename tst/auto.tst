#############################################################################
##
#W  auto.tst   Testing automorphisms             G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: auto.tst, v 0.99 2004/10/11 gap Exp $
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
Group([ (3,6)(4,5), (2,4)(5,7), (2,5)(4,7), (2,7)(4,5) ])
gap> Size( AutomorphismGroup( SteinerLoop( 16, 77 ) ) );
3

# MODIFICATIONS ARE TOO SPECIAL TO BE TESTED HERE

gap> STOP_TEST( "auto.tst", 10000000 );
