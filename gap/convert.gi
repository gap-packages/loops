#############################################################################
##
#W  convert.gi      Conversions, encoding and decoding [loops]
##  
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
##  of order in [1..91], using characters instead of numbers.
##  When n>2, first row and column are ignored because they can be 
##  reconstructed from the rest of the table.
##  When <ct> is commutative, only "half" of the table is saved.
##  This can be detected from <str> and decoded appropriately.

InstallMethod( LOOPS_EncodeCayleyTable, "for list",
    [ IsList ],
function( ct )
    local n, ret, start, i, j;
    n := Length( ct );
    ret := "";
    start := 2;
    # small cases
    if n<3 then 
        start := 1;
    fi;
    if n>91 then
        Error("LOOPS: Encoding of Cayley tables is supported only for order less than 92.");
    fi;
    ret := "";
    if ct <> TransposedMat( ct ) then # general case
        for i in [start..n] do for j in [start..n] do
            Add(ret, LOOPS_DigitToChar( ct[i][j]-1 ) );
        od; od;
    else # commutative case
        for i in [start..n] do for j in [i..n] do
            Add(ret, LOOPS_DigitToChar( ct[i][j]-1 ) );
        od; od;
    fi;
    return ret;
end );

InstallMethod( LOOPS_DecodeCayleyTable, "for string",
    [ IsString ],
function( str )
    local symbols, n, pos, ret, i, j;
    symbols := Set( Set( str ), c -> LOOPS_CharToDigit(c) + 1 );
    if Length(str)=1 then
        return [[LOOPS_CharToDigit(str[1])+1]];
    fi;
    if Length(str)=3 and Size(symbols)=2 then # n=2 (automatically commutative)
        return [
            [LOOPS_CharToDigit(str[1])+1, LOOPS_CharToDigit(str[2])+1],
            [LOOPS_CharToDigit(str[2])+1, LOOPS_CharToDigit(str[3])+1]
        ];
    fi;
    # n>2     
    n := Size( symbols ); # number of distinct symbols
    if n>91 then
        Error("LOOPS: Decoding of Cayley tables is supported only for order less than 92.");
    fi;
    ret := List([1..n], i -> List( [1..n], j -> -1 ) );
    # the table except for the first row and first column
    pos := 1;
    if Length(str) = (n-1)^2 then # noncommutative case
        for i in [2..n] do for j in [2..n] do
            ret[i][j] := LOOPS_CharToDigit( str[pos] ) + 1;
            pos := pos+1;
        od; od;
    else # commutative case
        for i in [2..n] do for j in [i..n] do
            ret[i][j] := LOOPS_CharToDigit( str[pos] ) + 1;
            ret[j][i] := ret[i][j];
            pos := pos + 1;
        od; od;
    fi;
    # determining the first row and first column
    for i in [2..n] do
        ret[i][1] := Difference( symbols, ret[i] )[1];
        ret[1][i] := Difference( symbols, List( [2..n], j->ret[j][i] ) )[1];
    od;
    ret[1][1] := Difference( symbols, ret[1] )[1];
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

#############################################################################
##  
#O  LOOPS_EncodeCocycle( coc, values ) 
#O  LOOPS_DecodeCocycle( ecoc, values ) 
##    
##  Given loops F and K, a cocycle is a mapping from F x F to K.
##  Let n=|F| and b=|K|.
##  Cocycles are represented as n x n arrays with entries in a b-element set.
##
##  These are auxiliary routines for encoding and decoding of cocycles.
##  <coc> is a cocycle, an n x n matrix
##  <values> is a set of cardinality b
##      every entry in <coc> must lie in <values> but not vice versa
##  <ecoc> is an encoded cocycle, a list of the form [n, is_comm, data],
##      where n=|F|, is_comm is true iff <coc> is commutative, and
##      data is an encoded cocycle table
##  Note: The encoded cocycle has default values in [0..b-1]. The argument
##  <values> can be used to populate the cocycle with other values.

InstallMethod( LOOPS_EncodeCocycle, "for two lists",
    [ IsList, IsList ],
function( coc, values )
    local b, n, is_commutative, ret, i, start, j;
    b := Length( values );
    if not b < 92 then
        Error("LOOPS: Encoding of cocycles is supported only for loops of order less than 92.");
    fi;
    n := Length(coc);
    is_commutative := coc = TransposedMat(coc);
    ret := [ n, is_commutative, "" ];
    for i in [1..n] do
        start := 1;
        if is_commutative then start := i; fi;
        for j in [start..n] do
            Add( ret[3], LOOPS_DigitToChar( Position( values, coc[i][j] ) - 1 ) );
        od;
    od;
    ret[3] := LOOPS_ConvertBase( ret[3], b, 91 );
    return ret;
end);

InstallMethod( LOOPS_DecodeCocycle, "for two lists",
    [ IsList, IsList ],
function( ecoc, values )
    local n, is_commutative, b, s, coc, pos, i, j;
    n := ecoc[1];
    is_commutative := ecoc[2];
    b := Length( values );
    if is_commutative then
        s := LOOPS_ConvertBase( ecoc[3], 91, b, n*(n+1)/2 );
    else
        s := LOOPS_ConvertBase( ecoc[3], 91, b, n^2 );
    fi;
    coc := List([1..n], i -> [1..n]);
    pos := 1;
    if is_commutative then
        for i in [1..n] do for j in [i..n] do
            coc[i][j] := values[ LOOPS_CharToDigit( s[pos] ) + 1 ];
            coc[j][i] := coc[i][j];
            pos := pos + 1;
        od; od;
    else
        for i in [1..n] do for j in [1..n] do
            coc[i][j] := values[ LOOPS_CharToDigit( s[pos] ) + 1 ];
            pos := pos + 1;
        od; od;
    fi;
    return coc;
end);

