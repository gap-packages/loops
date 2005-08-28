#############################################################################
##
#W  lib.tst   Testing libraries of loops         G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: lib.tst, v 0.9996 2005/8/22 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

gap> START_TEST("LOOPS, lib: testing all librairies except Moufang");

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
gap> t := Length( interesting_data[ 1 ] );
4

# testing loops  
gap> for i in [1..t] do
>       n := interesting_data[ 1 ][ i ];
>       for m in [ 1..interesting_data[ 2 ][ i ] ] do
>               InterestingLoop( n, m );
>       od;
> od;

# LEFT BOL LOOPS

gap> DisplayLibraryInfo( "left Bol" );
The library contains all left Bol loops of order 8 
that are not Moufang.
------
Extent of the library:
   6 loops of order 8
true

# number of orders implemented in the library
gap> t := Length( left_bol_data[ 1 ] );
1

# testing loops  
gap> for i in [1..t] do
>       n := left_bol_data[ 1 ][ i ];
>       for m in [ 1..left_bol_data[ 2 ][ i ] ] do
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
gap> t := Length( steiner_data[ 1 ] );
5

# testing loops  
gap> for i in [1..t] do
>       n := steiner_data[ 1 ][ i ];
>       for m in [ 1..steiner_data[ 2 ][ i ] ] do
>               SteinerLoop( n, m );
>       od;
> od;

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

gap> STOP_TEST( "lib.tst", 10000000 );
