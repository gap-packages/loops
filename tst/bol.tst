#############################################################################
##
#W  bol.tst   Testing Bol loops                 G. P. Nagy / P. Vojtechovsky
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##
gap> START_TEST("LOOPS, bol: testing methods for Bol loops");

# TESTING ASSOCIATED BRUCK LOOPS
gap> Q := LeftBolLoop(15,2);;
gap> B := AssociatedLeftBruckLoop(Q);;
gap> IsomorphismLoops(B,LeftBolLoop(15,1));
(7,9,10,8)(12,13,15,14)
gap> Q := RightBolLoop(15,1);;
gap> AssociatedRightBruckLoop( Q );
<right Bruck loop of order 15>

# TESTING EXACT GROUP FACTORIZATIONS
gap> G := SymmetricGroup( 5 );;
gap> H1 := Subgroup( G, [(1,2),(1,3),(1,4)] );;
gap> H2 := Subgroup(G,[(1,2,3,4,5)]);;
gap> IsExactGroupFactorization(G,H1,H2);
true
gap> RightBolLoopByExactGroupFactorization(G,H1,H2);
<loop of order 120>
gap> RightBolLoopByExactGroupFactorization([G,H1,H2]);
<loop of order 120>

#
gap> STOP_TEST( "bol.tst", 10000000 );
