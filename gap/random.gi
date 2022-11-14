#############################################################################
##
#W  random.gi     Random loops [loops]
##  
##  
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  
#O  RandomQuasigroup( n, iter ) 
##    
##  Returns a random quasigroup of order <n> using <iter> random steps to move into an
##  initial position in the Jacobson & Matthews method.
##  (MATH) This is an implementation of the Jacobson & Matthews random walk method with
##  some ad hoc mixing parameters. We always start with the cyclic group of order n.
##  It is proved in Jacobson & Matthews that a random walk in the graph visits all
##  latin squares uniformly. (But the problem is how to move to an initial position.)

InstallMethod( RandomQuasigroup, "for two integers",
   [ IsInt, IsInt ],
function( n, iter )                     
    local f, x, y, z, is_proper, xx, yy, zz, random_walk_step, i, ct;
    
    if not ( n > 0 and iter > 0 ) then
        Error("LOOPS: the arguments must be positive integers.");
    fi;
       
    # n=1 is a special case
    if n=1 then
        return LoopByCayleyTable( [[1]] );
    fi;

    # initializing function for proper and improper Latin squares
    # the meaning of f(x,y,z)=1 is that there is symbol z in row x and column y
    f := List([1..n], i -> List([1..n], j -> 0*[1..n]));
    # cyclic group of order n on symbols [1..n]
    for x in [1..n] do for y in [1..n] do 
        z := x+y-1;
        if z > n then
            z := z - n;
        fi;
        f[x][y][z] := 1;
    od; od;
    is_proper := true; # proper latin square to start with
    
    # one random walk step
    random_walk_step := function()
        local x, y, z, triples, x2, y2, z2, triple;
        if is_proper then
            repeat 
                x := Random([1..n]);
                y := Random([1..n]);
                z := Random([1..n]);
            until f[x][y][z]=0;
        fi;
        if not is_proper then # use unique point with f(x,y,z)=-1
            x := xx; 
            y := yy; 
            z := zz;
        fi;
        # find all suitable triples
        x2 := Filtered( [1..n], a -> f[a][y][z] = 1 );
        y2 := Filtered( [1..n], a -> f[x][a][z] = 1 );
        z2 := Filtered( [1..n], a -> f[x][y][a] = 1 );
        # pick a random suitable triple
        x2 := Random( x2 );
        y2 := Random( y2 );
        z2 := Random( z2 );
        # shuffle values
        f[x][y][z] := f[x][y][z] + 1;
        f[x][y2][z2] := f[x][y2][z2] + 1;
        f[x2][y][z2] := f[x2][y][z2] + 1;
        f[x2][y2][z] := f[x2][y2][z] + 1;
        f[x2][y][z] := f[x2][y][z] - 1;
        f[x][y2][z] := f[x][y2][z] - 1;
        f[x][y][z2] := f[x][y][z2] - 1;
        f[x2][y2][z2] := f[x2][y2][z2] - 1;
        # determine properness
        if f[x2][y2][z2] = 0 then
            is_proper := true;
        else
            is_proper := false;
            xx := x2;
            yy := y2;
            zz := z2;
        fi;
    end;
    
    # moving into an initial point in the graph
    for i in [1..iter] do
        random_walk_step();
    od;
    # finding a proper square nearby
    while not is_proper do
        random_walk_step();
    od;
    # constructing the multiplication table from the function
    ct := List([1..n], i->[1..n]);
    for x in [1..n] do for y in [1..n] do
        ct[x][y] := Filtered([1..n], z -> f[x][y][z] = 1)[ 1 ];
    od; od;
    return QuasigroupByCayleyTable( ct );
end);

#############################################################################
##  
#O  RandomQuasigroup( n ) 
##    
##  Returns random quasigroup of order <n> using n^3 steps to move into an
##  initial position in the Jacobson & Matthews algorithm.

InstallOtherMethod( RandomQuasigroup, "for a positive integer",
    [ IsInt ],
function( n )    
    return RandomQuasigroup( n, n^3 );
end);

#############################################################################
##  
#O  RandomLoop( n, iter ) 
##    
##  Returns a normalized random quasigroup of order <n>. 

InstallOtherMethod( RandomLoop, "for two positive integers",
    [ IsInt, IsInt ],
function( n, iter )
    local Q;
    Q := RandomQuasigroup( n, iter );
    return LoopByCayleyTable( NormalizedQuasigroupTable( CayleyTable( Q ) ) );
end);

#############################################################################
##  
#O  RandomLoop( n ) 
##    
##  Returns random loop of order <n> using n^3 steps to move into an
##  initial position in the Jacobson & Matthews algorithm.

InstallOtherMethod( RandomLoop, "for a positive integer",
    [ IsInt ],
function( n )
    local Q;
    Q := RandomQuasigroup( n );
    return LoopByCayleyTable( NormalizedQuasigroupTable( CayleyTable( Q ) ) );
end);

#############################################################################
##  
#O  RandomNilpotentLoop( lst ) 
##    
##  lst must be a list of positive integers and/or finite abelian groups.
##  If lst = [n] and n is an integer, returns a random abelian group of order n.
##  If lst = [A] and A is an abelian group, returns AsLoop( A ).
##  If lst = [a1,..,am] and a1 is an integer, returns a central extension
##      of an abelian group of order a1 by RandomNilpotentLoop( [a2,...,am] ).
##  If lst = [a1,..,am] and a1 is a group, returns a central extension
##      of a1 by RandomNilpotentLoop( [a2,...,am] ).
##  To determine the nilpotency class CL of the resulting loop, assume that
##  lst has length at least 2, contains only integers bigger than 1 (the "1" entries are trivial),
##  and let m be the last entry of lst. If m>2 then CL=Length(lst), else CL = Length(lst)-1.

InstallMethod( RandomNilpotentLoop, "for a list of abelian groups and positive integers",
    [ IsList ],
function( lst )                
    local n, K, F, f, theta, i, j, phi;
    if IsEmpty( lst ) then
        Error("LOOPS: the argument must be a list of finite abelian groups and/or positive integers.");
    fi;
    n := lst[1];
    if not ( IsPosInt( n ) or (IsGroup( n ) and IsAbelian( n ) and IsFinite( n ) ) ) then
        Error("LOOPS: the argument must be a list of finite abelian groups and/or positive integers.");
    fi;
    # central subloop
    if IsInt( n ) then # first argument is a positive integer
        K := IntoLoop( Random( AllSmallGroups( n, IsAbelian ) ) );
    else # first argument is an abelian group
        K := IntoLoop( n );
    fi;
    # factor loop
    if Length( lst ) = 1 then # trivial factor
        F := LoopByCayleyTable( [ [ 1 ] ] ); 
    else
        F := RandomNilpotentLoop( lst{[2..Length(lst)]} );
    fi;
    # cocycle (random)
    f := Size( F );
    theta := List([1..f], i->[1..f]);
    for i in [2..f] do
        theta[1][i]:=1;
    od;
    for i in [2..f] do for j in [2..f] do
        theta[i][j] := Random( [1..Size(K)] );
    od; od;
    # To guarantee that the resulting loop has maximal nilpotency class,
    # it suffices to make sure that theta is not symmetric.
    if f>2 then
        i := Random([2..f]);
        j := Random( Difference( [2..f], [i] ) );
        theta[i][j] := Random( Difference( [1..Size(K)], [ theta[j][i] ] ) );
    fi;
    # trivial action
    phi := List( [1..f], i->() );
    # the loop
    return LoopByExtension( K, F, phi, theta );
end);
