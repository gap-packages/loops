#############################################################################
##
#W  mouflib.tst   Testing Moufang loops library  G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: mouflib.tst, v 0.99 2004/09/28 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

# THIS TEST TAKES ABOUT 20 MINUTES TO COMPLETE !!!

gap> START_TEST("LOOPS, mouflib: testing the library of Moufang loops");

gap> DisplayLibraryInfo( "Moufang" );
The library contains all nonassociative Moufang loops 
of order less than 64, and 4262 nonassociative Moufang 
loops of order 64.
------
Extent of the library:
   1 loop of order 12
   5 loops of order 16
   1 loop of order 20
   5 loops of order 24
   1 loop of order 28
   71 loops of order 32
   4 loops of order 36
   5 loops of order 40
   1 loop of order 42
   1 loop of order 44
   51 loops of order 48
   1 loop of order 52
   2 loops of order 54
   4 loops of order 56
   5 loops of order 60
   4262 loops of order 64
true

# number of orders implemented in the library
gap> t := Length( moufang_data[ 1 ] );
16

# testing loops of order less than 64
gap> for i in [1..t-1] do
>       n := moufang_data[ 1 ][ i ];
>       for m in [ 1..moufang_data[ 2 ][ i ] ] do
>               MoufangLoop( n, m );
>       od;
> od;

# testing loops of order 64
# each 1000 loops take a few minutes
gap> for m in [1..1000] do MoufangLoop( 64, m ); od;
gap> for m in [1001..2000] do MoufangLoop( 64, m ); od;
gap> for m in [2001..3000] do MoufangLoop( 64, m ); od;
gap> for m in [3001..4000] do MoufangLoop( 64, m ); od;
gap> for m in [4000..4262] do MoufangLoop( 64, m ); od;

gap> STOP_TEST( "mouflib.tst", 10000000 );
