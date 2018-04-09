#############################################################################
##
#W  elements.gd  Elements and basic arithmetic operations [loops]
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  DISPLAYING AND COMPARING ELEMENTS
##  -------------------------------------------------------------------------

InstallMethod( PrintObj, "for a quasigroup element",
    [ IsQuasigroupElement ],
function( obj )
    local F;
    F := FamilyObj( obj );
    Print( F!.names, obj![ 1 ] );
end );

InstallMethod( PrintObj, "for a loop element",
    [ IsLoopElement ],
function( obj )
    local F;
    F := FamilyObj( obj );
    Print( F!.names, obj![ 1 ] );
end );

InstallMethod( \=, "for two elements of a quasigroup",
    IsIdenticalObj,
    [ IsQuasigroupElement, IsQuasigroupElement ],
function( x, y )
    return FamilyObj( x ) = FamilyObj( y ) and x![ 1 ] = y![ 1 ];
end );

InstallMethod( \<, "for two elements of a quasigroup",
    IsIdenticalObj,
    [ IsQuasigroupElement, IsQuasigroupElement ],
function( x, y )
    return FamilyObj( x ) = FamilyObj( y ) and x![ 1 ] < y![ 1 ];
end );

InstallMethod( \., "for quasigroup and positive integer",
    [ IsQuasigroup, IsPosInt ],
function( Q, k )
    return GeneratorsOfQuasigroup( Q )[ Int( NameRNam( k ) ) ];
end );


#############################################################################
##  MULTIPLICATION
##  -------------------------------------------------------------------------

##  Multiplication without parentheses is evaluated from left to right,
##  i.e., a*b*c=(a*b)*c. Powers use binary decomposition.
InstallMethod( \*, "for two quasigroup elements",
    IsIdenticalObj,
    [ IsQuasigroupElement, IsQuasigroupElement ],
function( x, y )
    local F;
    F := FamilyObj( x );
    return F!.set[ F!.cayleyTable[ x![ 1 ] ][ y![ 1 ] ] ];
end );

InstallOtherMethod( \*, "for a QuasigroupElement and a list",
    [ IsQuasigroupElement , IsList ],
function( x, ly )
    return List( ly, y -> x*y );
end );

InstallOtherMethod( \*, "for a list and a QuasigroupElement",
    [ IsList, IsQuasigroupElement ],
function( lx, y )
    return List( lx, x -> x*y );
end );

#############################################################################
##  DIVISION
##  -------------------------------------------------------------------------

##  z=x/y means zy=x
InstallMethod( RightDivision, "for two quasigroup elements",
    IsIdenticalObj,
    [ IsQuasigroupElement, IsQuasigroupElement ],
function( x, y )
    local F, ycol;
    F := FamilyObj( x );
    ycol := F!.cayleyTable{ [ 1 .. F!.size ] }[ y![ 1 ] ];
    return F!.set[ Position( ycol, x![ 1 ] ) ];
end );

InstallOtherMethod( RightDivision,
    "for a list and a quasigroup element",
    [ IsList, IsQuasigroupElement ],
    0,
function( lx, y )
    return List( lx, x -> RightDivision(x, y) );
end );

InstallOtherMethod( RightDivision,
    "for a quasigroup element and a list",
    [ IsQuasigroupElement, IsList ],
    0,
function( x, ly )
    return List( ly, y -> RightDivision(x, y) );
end );

InstallOtherMethod( \/,
   "for two elements of a quasigroup",
   IsIdenticalObj,
   [ IsQuasigroupElement, IsQuasigroupElement ],
   0,
function( x, y )
   return RightDivision( x, y );
end );

InstallOtherMethod( \/,
    "for a list and a quasigroup element",
    [ IsList, IsQuasigroupElement ],
    0,
function( lx, y )
    return List( lx, x -> RightDivision(x, y) );
end );

InstallOtherMethod( \/,
    "for a quasigroup element and a list",
    [ IsQuasigroupElement, IsList ],
    0,
function( x, ly )
    return List( ly, y -> RightDivision(x, y) );
end );

##  z = x\y means xz=y
InstallMethod( LeftDivision, "for two quasigroup elements",
    IsIdenticalObj,
    [ IsQuasigroupElement, IsQuasigroupElement ],
function( x, y )
    local F;
    F := FamilyObj( x );
    return F!.set[ Position( F!.cayleyTable[ x![ 1 ] ], y![ 1 ] ) ];
end );

InstallOtherMethod( LeftDivision,
    "for a list and a quasigroup element",
    [ IsList, IsQuasigroupElement ],
    0,
function( lx, y )
    return List( lx, x -> LeftDivision(x, y) );
end );

InstallOtherMethod( LeftDivision,
    "for a quasigroup element and a list",
    [ IsQuasigroupElement, IsList ],
    0,
function( x, ly )
    return List( ly, y -> LeftDivision(x, y) );
end );

#############################################################################
##
#O  LeftDivisionCayleyTable( Q )
##
##  Returns the Cayley table for the operation x\y of the quasigroup <Q>.

InstallMethod( LeftDivisionCayleyTable, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    # This would be slow using LeftDivision.
    # Must take care of the fact that entries in ct are not necessarily 1..n
    local n, ct, pos_in_Q, pos_in_parent, i, t, j;
    n := Size( Q );
    ct := CayleyTable( Q );
    pos_in_Q := 0*[ 1..Size( Parent( Q ) ) ];
    pos_in_parent := PosInParent( Q );
    for i in pos_in_parent do
        pos_in_Q[ i ] := Position( pos_in_parent, i );
    od;
    t := List( [1..n], i -> 0*[1..n] );
    for i in [1..n] do for j in [1..n] do
        t[ i ][ pos_in_Q[ ct[ i ][ j ] ] ] := pos_in_parent[ j ];
    od; od;
    return t;
end );

#############################################################################
##
#O  RightDivisionCayleyTable( Q )
##
##  Returns the Cayley table for the operation x/y of the quasigroup <Q>.

InstallMethod( RightDivisionCayleyTable, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    # This would be slow using RightDivision.
    # Must take care of the fact that entries in ct are not necessarily 1..n
    local n, ct, pos_in_Q, pos_in_parent, i, t, j;
    n := Size( Q );
    ct := CayleyTable( Q );
    pos_in_Q := 0*[ 1..Size( Parent( Q ) ) ];
    pos_in_parent := PosInParent( Q );
    for i in pos_in_parent do
        pos_in_Q[ i ] := Position( pos_in_parent, i );
    od;
    t := List( [1..n], i -> 0*[1..n] );
    for i in [1..n] do for j in [1..n] do
        t[ pos_in_Q[ ct[ i ][ j ] ] ][ j ] := pos_in_parent[ i ];
    od; od;
    return t;
end );

#############################################################################
##  POWERS AND INVERSES
##  -------------------------------------------------------------------------

InstallMethod( \^, "for a quasigroup element and a permutation",
    [ IsQuasigroupElement, IsPerm ],
function( x, p )
    local F;
    F := FamilyObj( x );
    return F!.set[ ( x![ 1 ] )^p ];
end );

InstallMethod( OneOp, "for loop elements",
    [ IsLoopElement ],
function( x )
    local F;
    F := FamilyObj( x );
    return F!.set[ 1 ];
end );

#############################################################################
##
#A  LeftInverse( <x> )
##
##  If <x> is a loop element, returns the left inverse of <x>

InstallMethod( LeftInverse, "for loop elements",
    [ IsLoopElement ],
    x -> RightDivision( One( x ), x )
);

#############################################################################
##
#A  RightInverse( <x> )
##
##  If <x> is a loop element, returns the left inverse of <x>

InstallMethod( RightInverse, "for loop elements",
    [ IsLoopElement ],
    x -> LeftDivision( x, One( x ) )
);

InstallMethod( InverseOp, "for loop elements",
    [ IsLoopElement ],
function( x )
    local y;
    y := RightInverse( x );
    if y = LeftInverse( x ) then return y; fi;
    return fail;
end );

#############################################################################
##  ASSOCIATORS AND COMMUTATORS
##  -------------------------------------------------------------------------

#############################################################################
##
#O  Associator( x, y , z )
##
##  When <x>, <y>, <z> are elements of a quasigroup Q, returns the
##  associator of <x>, <y>, <z>, i.e., the unique element u satisfying
##  (xy)z = (x(yz))u.

InstallMethod( Associator, "for three quasigroup elements",
    [ IsQuasigroupElement, IsQuasigroupElement, IsQuasigroupElement ],
function( x, y, z )
    return LeftDivision( x*(y*z), (x*y)*z );
end);

#############################################################################
##
#O  Commutator( x, y )
##
##  When <x>, <y> are elements of a quasigroup Q, returns the
##  commutator of <x>, <y>, i.e., the unique element u satisfying
##  (xy) = (yx)u.

InstallMethod( Commutator, "for two quasigroup elements",
    [ IsQuasigroupElement, IsQuasigroupElement ],
function( x, y )
    return LeftDivision( y*x, x*y );
end);
