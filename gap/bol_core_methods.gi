#############################################################################
##
#W  bol_core_methods.gi     Common methods for Bol loops [loops]
##
#H  @(#)$Id: bol_core_methods.gi, v 2.0.0 2008/03/06 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##
#A  AssociatedLeftBruckLoop( Q )
##
##  Given a left Bol loop Q for which x -> x^2 is a permutation,
##  returns the associated left Bruck loop, defined by 
##  xy = (x*((y*y)*x))^(1/2)  (default) or
##  xy = x^(1/2)*(y*x^(1/2))  (commented out)


InstallMethod( AssociatedLeftBruckLoop,
    [ IsLeftBolLoop ],
function( Q )
    local n, ct, squares, roots, new_ct, L, i, j;
    n := Size( Q );
    ct := CanonicalCayleyTable( CayleyTable( Q ) );
    squares := List( [ 1..n ], i -> ct[i][i] );
    if not Size( Set( squares ) ) = n then
        Error( "LOOPS: <1> must be a left Bol loop in which squaring is a bijection." );
    fi;
    roots := Inverse( PermList( squares ) ); # square roots
    new_ct := List([1..n], i -> [1..n] );
    for i in [1..n] do for j in [1..n] do
        new_ct[i][j] :=  (ct[i][ct[ct[j][j]][i]])^roots; # (x*((y*y)*x))^(1/2)
#       new_ct[i][j] := ct[i^roots][ct[ j ][ i^roots ]]; # x^(1/2)*(y*x^(1/2))
    od; od;
    L := LoopByCayleyTable( new_ct );   # the associated left Bruck loop
    SetIsLeftBruckLoop( L, true );
    return L;
end );

