#############################################################################
##
#W  convert.gi      Conversions, encoding and decoding [loops]
##  
#H  @(#)$Id: convert.gi, v 3.0.0 2015/06/02 gap Exp $
##  
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

############################################################################
##
## CONVERSIONS BETWEEN CHARACTERS AND "DIGITS" IN BASE 91.
## -------------------------------------------------------------------------
##
## Suitable characters to represent digits in GAP are found in the interval
## CHAR_INT(35)..CHAR_INT(126), except for CHAR_INT[92] = '\\'.
## This leads to natural base 91 = 126-35.
## To implement binary, decimal and hexadecimal numbers naturally,
## we reorder the suitable 91 characters to read as follows:

BindGlobal(
    "LOOPS_conversion_string",
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%&'()*+,-./:;<=>?@[]^_`{|}~"
);

#############################################################################
##
#O  LOOPS_DigitToChar( d )
##
##  Converts an integer <d> in the range [0..90] to a character.

InstallMethod( LOOPS_DigitToChar, "for integer",
    [ IsInt ],
function( d )
    return LOOPS_conversion_string[ d + 1 ];
end );


#############################################################################
##
#O  LOOPS_CharToDigit( c )
##
##  Converts a character <c> to an integer in the range [0..90].

InstallMethod( LOOPS_CharToDigit, "for character",
    [ IsChar ],
function( c )
    return Position( LOOPS_conversion_string, c ) - 1;
end );

#############################################################################
##  
#O  LOOPS_EncodeCayleyTable( ct ) 
#O  LOOPS_DecodeCayleyTable( str ) 
##    
##  Auxiliary routines for encoding and decoding of loop Cayley tables
##  up to order 91, using characters instead of numbers. First row and 
##  first column are ignored and assumed to be canonical.

InstallMethod( LOOPS_EncodeCayleyTable, "for list",
    [ IsList ],
function( ct )
    local n, i, j, ret;
    n := Length( ct );
    if n>91 then
        Error("LOOPS: Encoding of Cayley tables is supported only for order at most 91.");
    fi;
    ret := "";
    for i in [2..n] do for j in [2..n] do
        Add(ret, LOOPS_DigitToChar( ct[i][j]-1 ) );
    od; od;
    return ret;
end );

InstallMethod( LOOPS_DecodeCayleyTable, "for string",
    [ IsString ],
function( str )
    local n, pos, ret, i, j;
    n := Sqrt( Length( str ) ) + 1;
    pos := 1;
    ret := [[1..n]];
    for i in [2..n] do
        ret[i] := [i];
        for j in [2..n] do
            ret[i][j] := LOOPS_CharToDigit( str[pos] ) + 1;
            pos:=pos+1;
        od;
    od;
    return ret;
end );

#############################################################################
##
#O  LOOPS_ConvertToDecimal( s, n )
##
##  Converts an <n>-ary number represented by a string <s> to a decimal
##  number represented as integer.

InstallMethod( LOOPS_ConvertToDecimal, "for string and integer",
    [ IsString, IsInt ],
function( s, n )
    local ls, d, i;
    ls := Length( s );
    d := 0;
    for i in [1..ls] do
        d := d + LOOPS_CharToDigit(s[i])*(n^(ls-i));
    od;
    return d;
end );


#############################################################################
##
#F  LOOPS_ConvertFromDecimal( arg )
##
##  arg = [ <d>, <m>, optional <k> ]
##  Converts a decimal number <d> to a number in base <m>.
##  Optional parameter <k> is the minimal required number of "digits"
##      of the output in new base <m>, including zeros at the beginning
##  Returns the corresponding number as a string in base <m> (of length at least <k>).

InstallGlobalFunction( LOOPS_ConvertFromDecimal, function( arg )
    local d, m, s, r, prefix_s;
    d := arg[1];
    m := arg[2];
    s := "";
    while d>0 do
        r := d mod m;
        Add( s, LOOPS_DigitToChar( r ) );
        d := (d-r)/m;
    od;
    s := Reversed( s );
    if Length( arg ) > 2 then
        prefix_s := List( [1..arg[3]-Length(s)], i -> LOOPS_DigitToChar( 0 ) );
        s := Concatenation( prefix_s, s );
    fi;
    return s;
end );


#############################################################################
##
#F  LOOPS_ConvertBase( arg )
##
##  arg = [ s, n, m, optional k ]
##  s is a string that represents a number in base n
##  m is a new base
##  optional parameter k is the minimal required number of "digits"
##      of the output in new base m, including zeros at the beginning
##  Returns the corresponding number in base m (with at least k digits).

InstallGlobalFunction( LOOPS_ConvertBase, function( arg )
    local d;
    d := LOOPS_ConvertToDecimal( arg[1], arg[2] );
    if Length(arg)>3 then
        return LOOPS_ConvertFromDecimal( d, arg[3], arg[4] );
    fi;
    return LOOPS_ConvertFromDecimal( d, arg[3] );
end );
