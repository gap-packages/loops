#############################################################################
##
#W  lib.tst   Testing libraries of loops         G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: lib.tst, v 3.0.0 2015/06/15 gap Exp $
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
   1 loop of order 96
true

# number of orders implemented in the library
gap> t := Length( LOOPS_interesting_data[ 1 ] );
5

# testing loops  
gap> for i in [1..t] do
>       n := LOOPS_interesting_data[ 1 ][ i ];
>       for m in [ 1..LOOPS_interesting_data[ 2 ][ i ] ] do
>               InterestingLoop( n, m );
>       od;
> od;

# LEFT/RIGHT BOL LOOPS

gap> DisplayLibraryInfo( "left Bol" );
The library contains all nonassociative left Bol loops of order less than 17.
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

# testing right Bol loop
gap> RightBolLoop( 8, 1 );
<right Bol loop 8/1>

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

# RCC LOOPS
gap> DisplayLibraryInfo("RCC");
The library contains all nonassociative RCC loops of order less than 28.
------
Extent of the library:
   3 loops of order 6
   19 loops of order 8
   5 loops of order 9
   16 loops of order 10
   155 loops of order 12
   97 loops of order 14
   17 loops of order 15
   6317 loops of order 16
   1901 loops of order 18
   8248 loops of order 20
   119 loops of order 21
   10487 loops of order 22
   471995 loops of order 24
   119 loops of order 25
   151971 loops of order 26
   152701 loops of order 27
true

gap> RCCLoop(6,1); RCCLoop(16,6317); RightConjugacyClosedLoop(27,152701);
<RCC loop 6/1>
<RCC loop 16/6317>
<RCC loop 27/152701>

gap> LCCLoop(6,3); LCCLoop(25,119);
<LCC loop 6/3>
<LCC loop 25/119>

# CC LOOPS

gap> DisplayLibraryInfo("CC");
The library contains all nonassociative CC loops of order less than 28
and all nonassociative CC loops of order p^2 and 2*p for any odd prime p.
------
Extent of the library:
   2 loops of order 8
   3 loops of order 12
   28 loops of order 16
   7 loops of order 18
   3 loops of order 20
   1 loop of order 21
   14 loops of order 24
   55 loops of order 27
   3 loops of order p^2 for every odd prime p,
   1 loop of order 2*p for every odd prime p
true

gap> CCLoop(25,1); CCLoop(49,2); CCLoop(121,3); CCLoop(14,1);
<CC loop 25/1>
<CC loop 49/2>
<CC loop 121/3>
<CC loop 14/1>

gap> CCLoop(16,28); ConjugacyClosedLoop(27,55);
<CC loop 16/28>
<CC loop 27/55>

# SMALL LOOPS

gap> DisplayLibraryInfo("small");
The library contains all nonassociative loops of order less than 7.
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
The library contains all nonassociative even code loops
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
