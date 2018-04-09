#############################################################################
##
#W  nilpot.tst   Testing nilpotency              G. P. Nagy / P. Vojtechovsky
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##
gap> START_TEST("LOOPS, nilpot: nilpotency and triality group");

# GENERAL NILPOTENCY
gap> L := LoopByCayleyTable( 
> [ [ 1, 2, 3, 4, 5, 6, 7, 8 ], [ 2, 1, 4, 3, 6, 5, 8, 7 ], 
>   [ 3, 4, 1, 2, 7, 8, 5, 6 ], [ 4, 6, 2, 8, 1, 7, 3, 5 ], 
>   [ 5, 3, 7, 1, 8, 2, 6, 4 ], [ 6, 5, 8, 7, 2, 1, 4, 3 ], 
>   [ 7, 8, 5, 6, 3, 4, 1, 2 ], [ 8, 7, 6, 5, 4, 3, 2, 1 ] ] );
<loop of order 8>
gap> Center(L);
<associative loop of order 2>
gap> LeftNucleus(L);
<associative loop of order 2>
gap> RightNucleus(L); 
<associative loop of order 4>
gap> IsNilpotent(L);
true
gap> NilpotencyClassOfLoop(L);
2
gap> IsomorphismLoops(L,LeftBolLoop(8,2));
(3,8,4,6,5,7)

# NILPOTENCY FOR MOUFANG LOOPS
gap> L:=MoufangLoop(24,1);
<Moufang loop 24/1>
gap> Center(L);
<associative loop of order 2>
gap> IsNilpotent(L);
false
gap> NilpotencyClassOfLoop(L);
fail
gap> L:=MoufangLoop(32,32);
<Moufang loop 32/32>
gap> Center(L);
<associative loop of order 2>

# TRIALITY GROUPS
gap> tr:=TrialityPermGroup(L);;
gap> [ 33^tr.rho, 33^tr.sigma ];
[ 65, 1 ]
gap> Size(Center(tr.group));
4
gap> tr_pc:=TrialityPcGroup(L);  
rec( group := <Triality pc group of order 2^15>, rho := f2, sigma := f1 )
gap> Size(Centralizer(tr.group,tr.sigma));
1024
gap> Size(Centralizer(tr.group,tr.rho));  
32
gap> TrialityPermGroup(PSL(2,5));;
gap> Size(last.group);
216000

#
gap> STOP_TEST( "nilpot.tst", 10000000 );
