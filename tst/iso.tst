#############################################################################
##
#W  iso.tst   Testing isomorphisms             G. P. Nagy / P. Vojtechovsky
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##
gap> START_TEST("LOOPS, iso: testing isomorphisms");

# TESTING DISCIMINATOR
gap> Length( Discriminator( MoufangLoop( 12, 1 ) )[ 1 ] );
3

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
gap> Q := QuasigroupByCayleyTable([[3,2,1],[2,1,3],[1,3,2]]);;
gap> AutomorphismGroup( Q );
Group([ (1,2,3), (1,3,2) ])

# TESTING ISOMORPHISMS
gap> Q := DirectProduct( MoufangLoop( 32, 5 ) );;
gap> Qp := IsomorphicCopyByPerm( Q, (2,3,4)(17,20) );;
gap> Qq := LoopIsomorph( Q, (2,3,4)(17,20) );;
gap> Qp = Qq;
false
gap> CayleyTable( Qp ) = CayleyTable( Qq );
true
gap> IsomorphismLoops( Q, Qp );
(2,3,4)(18,23)(19,25)(21,27)(22,28)(24,30)(26,31)(29,32)
gap> LoopsUpToIsomorphism( [Q,Qp] );
[ <Moufang loop 32/5> ]
gap> Q2 := QuasigroupByCayleyTable( [[2,1],[1,2]] );;
gap> IsomorphismQuasigroups( Q2, IntoLoop(CyclicGroup(2)) );
(1,2)
gap> Length( QuasigroupsUpToIsomorphism( [Q,Q2] ) );
2

# TESTING ISOTOPISMS
gap> IsotopismLoops( SmallLoop( 5, 1 ), SmallLoop( 5, 4 ) );    
[ (3,4,5), (1,2), (1,2) ]

#
gap> STOP_TEST( "iso.tst", 10000000 );
