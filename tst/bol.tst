#############################################################################
##
#W  bol.tst   Testing Bol loops                 G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: iso.tst, v 2.0.0 2008/03/06 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

gap> START_TEST("LOOPS, bol: testing methods for Bol loops");

# TESTING ASSOCIATED BRUCK LOOPS

gap> Q := LeftBolLoop(15,2);;
gap> B := AssociatedLeftBruckLoop(Q);;
gap> IsomorphismLoops(B,LeftBolLoop(15,1));
(4,12,15,11)(5,9)(6,14)

gap> STOP_TEST( "bol.tst", 10000000 );
