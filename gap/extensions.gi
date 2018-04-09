#############################################################################
##
#W  extensions.gi     Extensions [loops]
##  
##  
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  
#O  LoopByExtension( K, F, phi, theta ) 
##    
##  Let K be an abelian group, F a loop, phi: F --> Aut( K ) a homomorphism,
##  and theta: F x F --> K a map (cocycle) satisfying theta(1,x) = theta(x,1)
##  = 1. Then the function returns the extension K x F with multiplication *
##  defined by (a,x)*(b,y) = (a   phi(x)(b)   theta(x,y),  xy).
##  
##  Note about arguments: 
##      phi is a list of length |F| of permutations of [1, ..., K ],
##      theta is am |F| by |F| matrix of numbers from [1, ..., K].

InstallMethod( LoopByExtension, "for a commutative associative loop and a loop",
    [ IsLoop, IsLoop, IsList, IsMatrix ],
function( K, F, phi, theta )                              
    local nK, nF, ctK, ctF, ct, a, b, x, y, c, z;
    # only some properties of the arguments are checked
    if not IsAssociative( K ) then Error("LOOPS: <1> must be an associative loop."); fi;
    if not IsCommutative( K ) then Error("LOOPS: <1> must be a commutative loop."); fi;
    # sizes
    nK := Size( K );
    nF := Size( F );
    # making sure all is canonical
    ctK := CanonicalCayleyTable( CayleyTable( K ) );
    ctF := CanonicalCayleyTable( CayleyTable( F ) );
    # future Cayley table
    ct := List([1..nK*nF], i -> 0 * [1..nK*nF] );
    # constructing the Cayley table
    for a in [1..nK] do for b in [1..nK] do for x in [1..nF] do for y in [1..nF] do
        c := ctK[ a ][ b^phi[x] ];
        c := ctK[ c ][ theta[ x ][ y ] ];
        z := ctF[ x ][ y ];
        ct[ nK*(x-1) + a ][ nK*(y-1) + b ] := nK*(z-1) + c;
    od; od; od; od;
    return LoopByCayleyTable( ct );
end);

#############################################################################
##  
#O  NuclearExtension( Q, K ) 
##    
##  Let Q be a loop and K an abelian normal subloop of Q contained in the 
##  nucleus of Q. Then Q is an extension of K by Q/K = F via a map 
##  phi: F --> Aut( K ) and a cocycle theta: F x F --> K, as above. 
##  This function returns the for ingredients [ K, F, phi, theta ], 
##  all in a canonical form.

InstallMethod( NuclearExtension, "for a loop and a normal nuclear subloop",
    [ IsLoop, IsLoop ],
function( Q, K )                
    local F, t, phi, theta, i, j, elmK;              
    # checking arguments
    if not IsNormal( Q, K ) then Error("LOOPS: <2> must be a normal subloop of <1>."); fi;
    if not IsCommutative( K ) then Error("LOOPS: <2> must be a commutative loop."); fi;
    if not ForAll( K, k -> k in Nuc( Q ) ) then Error("LOOPS: <2> must be contained in the nucleus of <1>."); fi;
    # partitioning Q nicely into cosets of K (not needed but it aids in visualization)
    Q := IsomorphicCopyByNormalSubloop( Q, K );
    K := Subloop( Q, [1..Size(K)] );
    F := FactorLoop( Q, K );
    t := List( [1..Size(F)], i -> Elements(Q)[(i-1)*Size(K) + 1] ); #transversal
    phi := List( t, x -> RestrictedPerm( MiddleInnerMapping( Q, x ), [1..Size(K)] ) ); #action
    theta := List( [1..Size(F)], i -> [1..Size(F)] ); #cocycle
    for i in [1..Size(F)] do for j in [1..Size(F)] do
        elmK := RightDivision( t[i]*t[j], t[ CayleyTable( F )[ i ][ j ] ] );
        theta[ i ][ j ] := Position( Elements( K ), elmK );
    od; od;
    return [ K, F, phi, theta ];
end);
