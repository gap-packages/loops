#############################################################################
##
#W  lib.tst   Testing libraries of loops         G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: lib.tst, v 1.9.0 2007/08/11 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

gap> START_TEST("LOOPS, lib: testing all libraries except Moufang");

# INTERESTING LOOPS

gap> DisplayLibraryInfo( "interesting" );
The library contains a few interesting loops.
------
Extent of the library:
   1 loop of order 5
   1 loop of order 6
   1 loop of order 16
   1 loop of order 32
true

# number of orders implemented in the library
gap> t := Length( LOOPS_interesting_data[ 1 ] );
4

# testing loops  
gap> for i in [1..t] do
>       n := LOOPS_interesting_data[ 1 ][ i ];
>       for m in [ 1..LOOPS_interesting_data[ 2 ][ i ] ] do
>               InterestingLoop( n, m );
>       od;
> od;

# LEFT BOL LOOPS

gap> DisplayLibraryInfo( "left Bol" );
The library contains all nonassociative left Bol loops of order less than 17,
including Moufang loops.
------
Extent of the library:
   6 loops of order 8
   3 loops of order 12
   2 loops of order 15
   2038 loops of order 16
true

# number of orders implemented in the library
gap> t := Length( LOOPS_left_bol_data[ 1 ] );
4

# testing loops  
gap> for i in [1..t] do
>       n := LOOPS_left_bol_data[ 1 ][ i ];
>       for m in [ 1..LOOPS_left_bol_data[ 2 ][ i ] ] do
>               LeftBolLoop( n, m );
>       od;
> od;

# STEINER LOOPS

gap> DisplayLibraryInfo( "Steiner" );
The library contains all nonassociative Steiner loops 
of order less or equal to 16. It also contains the 
associative Steiner loops of order 4 and 8. 
------
Extent of the library:
   1 loop of order 4
   1 loop of order 8
   1 loop of order 10
   2 loops of order 14
   80 loops of order 16
true

# number of orders implemented in the library
gap> t := Length( LOOPS_steiner_data[ 1 ] );
5

# testing loops  
gap> for i in [1..t] do
>       n := LOOPS_steiner_data[ 1 ][ i ];
>       for m in [ 1..LOOPS_steiner_data[ 2 ][ i ] ] do
>               SteinerLoop( n, m );
>       od;
> od;

# NILPOTENT LOOPS

gap> DisplayLibraryInfo( "nilpotent" );
The library contains all nonassociative nilpotent loops
of order less than 12.
------
Extent of the library:
   2 loops of order 6
   134 loops of order 8
   8 loops of order 9
   1043 loops of order 10
true

gap> NilpotentLoop( 10, 1000 );
<nilpotent loop 10/1000>

# PAIGE LOOPS

gap> DisplayLibraryInfo( "Paige" );
The library contains the smallest nonassociative finite 
simple Moufang loop.
------
Extent of the library:
   1 loop of order 120
true

gap> PaigeLoop( 2 );
<Paige loop 120/1>

# CC-LOOPS

gap> DisplayLibraryInfo("CC");
The library contains all nonassociative CC-loops
of order p^2 and 2*p for any odd prime p.
There are precisely 3 such loops of order p^2,
and precisely 1 such loop of order 2*p.
------
Extent of the library:
 3 loops of order p^2 and 1 loop of order 2*p
 for every odd prime p
true

gap> CCLoop(25,1); CCLoop(49,2); CCLoop(121,3); CCLoop(14,1);
<CC loop 25/1>
<CC loop 49/2>
<CC loop 121/3>
<CC loop 14/1>

# SMALL LOOPS

gap> DisplayLibraryInfo("small");
The library contains all nonassocaitive loops of order less than 7.
------
Extent of the library:
   5 loops of order 5
   107 loops of order 6
true

gap> SmallLoop( 5, 3 ); SmallLoop( 6, 12 );
<small loop 5/3>
<small loop 6/12>

# ITP SMALL LOOPS

gap> DisplayLibraryInfo("itp small");
The library contains all nonassociative loops of order less than 7 up to isoto\
pism.
------
Extent of the library:
   1 loop of order 5
   20 loops of order 6
true

gap> ItpSmallLoop( 5, 1 ); ItpSmallLoop( 6, 14 );
<small loop 5/1>
<small loop 6/42>

# CODE LOOPS

gap> DisplayLibraryInfo("code");
The library contains all nonasscoiative even code loops
of order less than 65.
------
Extent of the library:
   5 loops of order 16
   16 loops of order 32
   80 loops of order 64
true

gap> CodeLoop( 16, 3 );
<Moufang loop 16/3>
gap> CodeLoop( 64, 80 );
<Moufang loop 64/4247>

# AUTOMORPHIC LOOPS
gap> DisplayLibraryInfo("automorphic");
The library contains all nonassociative automorphic loops
of order less than 16.
------
Extent of the library:
   1 loop of order 6
   7 loops of order 8
   3 loops of order 10
   2 loops of order 12
   5 loops of order 14
   2 loops of order 15
true

gap> AutomorphicLoop(15,2);
<automorphic loop 15/2>

gap> STOP_TEST( "lib.tst", 10000000 );
