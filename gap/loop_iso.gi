#############################################################################
##
#W  loop_iso.gi    Isomorphisms & isotopisms of loops     Nagy / Vojtechovsky
##  
#H  @(#)$Id: loop_iso.gi, v 1.5.0 2007/03/21 gap Exp $
##  
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  DISCRIMINATOR
##  -------------------------------------------------------------------------

#############################################################################
##  
#O  Discriminator( L ) 
##    
##  Returns the dicriminator of a loop <L>.
##  Discriminator must be cheap to calculate, yet it is supposed to 
##  provide such invariants that result in a fine partition of <L> 
##  preserved under isomorphisms.

InstallMethod( Discriminator, "for loop",
    [ IsLoop ],
function( L )
    local n, T, I, i, j, k, ebo, c, J, counter, A, P, B, FrequencySet;
    
    # making sure loop table is canonical
    if L = Parent( L ) then T := CayleyTable( L );
    else T := CanonicalCayleyTable( CayleyTable( L ) ); fi;
    n := Size( L );
    # Calculating invariants.
    if not IsPowerAssociative( L ) then 
        # not power associative loop, hence crude discriminator
        # Element x asks: Am I neutral element? 
        # PROG: This is needed to make sure that the neutral element will be in a block by itself.
        I := List( [1..n], i -> [false, 0, 0, 0 ] );
        I[1] := [true, 0, 0, 0];
        # Element x asks: Am I an involution?
        for i in [1..n] do
            I[ i ][ 2 ] := T[ i ][ i ] = 1;
        od;
        # Element x asks: How many times am I a square?
        for i in [1..n] do
            j := T[ i ][ i ];
            I[ j ][ 3 ] := I[ j ][ 3 ] + 1;
        od;
        # Element x asks: With how many elements do I commute?
        for i in [1..n] do
            for j in [1..n] do if T[ i ][ j ] = T[ j ][ i ] then 
                I[ i ][ 4 ] := I[ i ][ 4 ] + 1; 
            fi; od;
        od;
    else    
        #power associative loop, hence refined discriminator
        # Element x asks: What is my order?
        I := List( L, x -> [Order(x), 0, 0] );
        # Element x asks: How many times am I a square, a fourth power?
        for i in [1..n] do
            j := T[ i ][ i ];
            I[ j ][ 2 ] := I[ j ][ 2 ] + 1;
            k := T[ j ][ j ];
            I[ k ][ 3 ] := I[ k ][ 3 ] + 1;
        od;
        # Element x asks: With how many elements of given order do I commute?
        ebo := List( [1..n], i -> []); # elements by order
        for i in [1..n] do Add( ebo[ I[ i ][ 1 ] ], i ); od;
        ebo := Filtered( ebo, i -> not IsEmpty( i ) );
        for i in [1..n] do
            c := [];
            for J in ebo do 
                counter := 0;
                for j in J do if T[ i ][ j ] = T[ j ][ i ] then 
                    counter := counter + 1; 
                fi; od;
                Add( c, counter );
            od;
            I[i][4] := c;
        od;
    fi; # All invariants have been calculated at this point.
    
    FrequencySet := function (L)
    # Auxiliary function. 
    # Given a list L, returns [ S, F ], where S = Set( L ),
    # and where F[ i ] is the number of occurences of Elements( S )[ i ] in L
        local S, F, x, i;
        S := Set( L );
        F := 0*[ 1..Size( S ) ];
        for x in L do
            i := Position( S, x );
            F[ i ] := F[ i ] + 1;
        od;
        return [S, F];
    end;

    # Setting up the first part of discriminator (invariants).
    A := FrequencySet( I );
    P := Sortex( A[ 2 ] ); #small invariant sets will be listed first
    A[ 1 ] := Permuted( A[ 1 ], P );
    
    # Setting up the second part of discriminator (blocks of elements invariant under isomorphisms).
    B := List( A[ 1 ], i -> [] ); #for every invariant get a list of elements    
    for i in [1..n] do
        Add( B[ Position( A[ 1 ], I[ i ] ) ],  Elements( L )[ i ] );
    od;
    
    # Returning the discriminator.
    return [ A, B ];
end);

#############################################################################
##  
#F  EfficientGenerators( L, D ) 
##    
##  Auxiliary function. 
##  Given a loop <L> with discriminator <D>, it returns a list of generators 
##  of <L> obtained by greedy algorithm from the partition <D>[ 2 ] of <L>.

EfficientGenerators := function( L, D ) 
    local A, i, gens, S;
    if Size( L ) = 1 then return [ One( L ) ]; fi;
    A := [];
    for i in [2..Length( D[ 2 ] )] do Append( A, D[ 2 ][ i ] ); od;
    gens := [];
    while not IsEmpty( A ) do
        Add( gens, A[ 1 ] );
        S := Subloop( L, gens );
        A := Filtered( A, x -> not x in S );
    od;
    return gens;
end;

#############################################################################
##  
#O  AreEqualDicriminators( D, E ) 
##    
##  Returns true if the loops corresponding to <D> and <E> are possibly
##  isomorphic. Returns false if the loops are certainly nonisomorphic.
##  It boils dwn to this: Discriminators are considered equal if they contain
##  the same invariants with the same frequency.
  
InstallMethod( AreEqualDiscriminators, "for two lists (discrimninators)",
    [ IsList, IsList ],
function( D, E )
      return D[ 1 ] = E[ 1 ];
end);

#############################################################################
##  EXTENDING MAPPINS (AUXILIARY)
##  -------------------------------------------------------------------------

# Here, we identity the map map f: A --> B  with the triple [ m, a, b ], 
# where a is a subset of A, b[ i ] is the image of a[ i ], and m[ i ] > 0
# if and only if i is in a.

#############################################################################
##  
#F  ExtendHomomorphismByClosingSource( f, L, M ) 
##
##  Auxiliary.    
##  <L>, <M> are multiplication tables of loops, <f> is a partial map
##  from a subset of elements of <L> to a subset of elements of <M>. 
##  This function attempts to extend <f> into a homomorphism of loops by 
##  extending the source of <f> into (the smallest possible) subloop of <L>.

ExtendHomomorphismByClosingSource := function( f, L, M )

    local oldS, newS, pairs, x, y, newNow, p, z, fz;    
    oldS := [ ];
    newS := f[ 2 ];

    repeat  
        pairs := [];
        for x in oldS do for y in newS do 
            Add( pairs, [ x, y ] ); 
            Add( pairs, [ y, x ] );
        od; od;
        for x in newS do for y in newS do
            Add( pairs, [ x, y ] );
        od; od;
        newNow := [];
        for p in pairs do
            x := p[ 1 ];
            y := p[ 2 ];
            z := L[ x ][ y ];
            fz := M[ f[ 1 ][ x ] ][ f[ 1 ][ y ] ];
            if f[ 1 ][ z ] = 0 then
                f[ 1 ][ z ] := fz; AddSet( f[ 2 ], z ); AddSet( f[ 3 ], fz );
                Add( newNow, z );
            else 
                if not f[ 1 ][ z ] = fz then return fail; fi;
            fi;
        od;
        oldS := Union( oldS, newS );
        newS := ShallowCopy( newNow );
    until IsEmpty( newS );
    return f;           
end;

#############################################################################
##  
#F  SublistPosition( S, x ) 
##  
##  auxiliary function  
##  input: list of lists <S>, element <x>
##  returns: smallest i such that x in S[i]; or fail.

SublistPosition := function( S, x )
    local i;
    for i in [ 1..Length( S ) ] do if x in S[ i ] then return i; fi; od;
    return fail;
end;

#############################################################################
##  
#F  ExtendIsomorphism( f, L, GenL, DisL, M, DisM ) 
##  
##  Auxiliary.  
##  Given a partial map <f> from loop <L> to loop <M>, it attempts to extend
##  <f> into an isomorphism betweem <L> and <M>.
##  <GenL>, <DisL> and <DisM> are precalculated and mean:
##  efficient generators of <L>, disriminator of <L>, efficient generators
##  of <M>, respectively.

ExtendIsomorphism := function( f, L, GenL, DisL, M, DisM )
    local x, possible_images, y, g;
    f := ExtendHomomorphismByClosingSource( f, L, M );
    if f = fail or Length( f[ 2 ] ) > Length( f[ 3 ] ) then return fail; fi;
    if Length( f[ 2 ] ) = Length( L ) then return f; fi; #isomorphism found
    
    x := GenL[ 1 ];
    GenL := List( [ 2..Length( GenL ) ], i -> GenL[ i ] ); 
    possible_images := Filtered( DisM[ SublistPosition( DisL, x ) ], y -> not y in f[ 3 ] );    
    for y in possible_images do
        g := StructuralCopy( f );
        g[ 1 ][ x ] := y; AddSet( g[ 2 ], x ); AddSet( g[ 3 ], y );
        g := ExtendIsomorphism( g, L, GenL, DisL, M, DisM );
        if not g = fail then return g; fi; #isomorphism found
    od;
    return fail;    
end;

#############################################################################
##  ISOMORPHISMS OF LOOPS
##  -------------------------------------------------------------------------

#############################################################################
##  
#O  IsomorphismLoopsNC( L, GenL, DisL, M, DisM ) 
##    
##  Auxiliary. Given a loop <L>, its efficient generators <DisL>, the 
##  disciminator <DisL> of <L>, and another loop <M> with discriminator
##  <DisM>, it returns an isomorophism from <L> onto <M>, or it fails.

IsomorphismLoopsNC := function( L, GenL, DisL, M, DisM )
    local map, iso;
    if not AreEqualDiscriminators( DisL, DisM ) then return fail; fi;

    #convert everything to numbers here (faster in GAP)
    DisL := List( DisL[2], D -> List( D, x -> Position( Elements( L ), x ) ) );
    DisM := List( DisM[2], D -> List( D, x -> Position( Elements( M ), x ) ) );
    GenL := List( GenL, x -> Position( Elements( L ), x ) );

    #mapping
    map := 0 * [ 1.. Size( L ) ]; map[ 1 ] := 1;
   
    iso := ExtendIsomorphism( [ map, [ 1 ], [ 1 ] ], CayleyTable( L ), GenL, DisL, CayleyTable( M ), DisM );
    if not iso = fail then return SortingPerm( iso[ 1 ] ); fi;
    return fail;
end;

#############################################################################
##  
#O  IsomorphismLoops( L, M ) 
##
##  If the loops <L>, <M> are isomorophic, it returns an isomorphism from
##  <L> onto <M>. Fails otherwise.

InstallMethod( IsomorphismLoops, "for two loops",
    [ IsLoop, IsLoop ],
function( L, M )
    local GenL, DisL, DisM;
    # making sure the loops have canonical Cayley tables
    if not L = Parent( L ) then L := LoopByCayleyTable( CayleyTable( L ) ); fi;    
    if not M = Parent( M ) then M := LoopByCayleyTable( CayleyTable( M ) ); fi;    
    DisL := Discriminator( L );
    GenL := EfficientGenerators( L, DisL );
    DisM := Discriminator( M );
    return IsomorphismLoopsNC( L, GenL, DisL, M, DisM );
end);

#############################################################################
##  
#O  LoopsUpToIsomorphism( ls ) 
##
##  Given a list <ls> of loops, returns a sublist of <ls> consisting
##  of represenatives of isomorphism classes of <ls>.

InstallMethod( LoopsUpToIsomorphism, "for a list of loops",
    [ IsList ],
function( ls )
    local loops, L, D, G, with_same_D, is_new_loop, K;
    # making sure only loops are on the list
    if not IsEmpty( Filtered( ls, x -> not IsLoop( x ) ) ) then
        Error("<arg1> must be a list of loops");
    fi;        
    loops := [];
    for L in ls do
        D := Discriminator( L );
        G := EfficientGenerators( L, D );
        # will be testing only loops with the same discriminator
        with_same_D := Filtered( loops, K -> AreEqualDiscriminators( K[2], D ) );
        is_new_loop := true;
        for K in with_same_D do
            if not IsomorphismLoopsNC( L, G, D, K[1], K[2] ) = fail then
                is_new_loop := false;
                break;
            fi;
        od;
        if is_new_loop then Add( loops, [ L, D ] ); fi;
    od;
    # returning only loops, not their discriminators
    return List( loops, L -> L[1] );
end);

#############################################################################
##  
#O  IsomorphicCopyByPerm( Q, p ) 
##
##  If <Q> is a quasigroup of order n and <p> a permutation of [1..n], returns
##  the quasigroup (Q,*) such that p(xy) = p(x)*p(y).
##  If <Q> is a loop, p is first composed with (1,1^p) to make sure
##  that the neutral element of (Q,*) remains 1.

InstallMethod( IsomorphicCopyByPerm, "for a quasigroup and permutation",
    [ IsQuasigroup, IsPerm ],
function( Q, p )
    local ctQ, ct, inv_p;
    ctQ := CanonicalCayleyTable( CayleyTable( Q ) );
    # if Q is a loop and 1^p > 1, must normalize
    if (IsLoop( Q ) and (not 1^p = 1)) then 
        p := p * (1, 1^p );
    fi;        
    inv_p := Inverse( p );
    ct := List([1..Size(Q)], i-> List([1..Size(Q)], j -> 
        ( ctQ[ i^inv_p ][ j^inv_p ] )^p 
    ) );
    if IsLoop( Q ) then return LoopByCayleyTable( ct ); fi;
    return QuasigroupByCayleyTable( ct );
end);

#############################################################################
##  
#O  IsomorphicCopyByNormalSubloop( L, S ) 
##
##  Given a loop <L> and its normal subloop <S>, it returns an isomorphic 
##  copy of <L> with elements reordered according to right cosests of S

InstallMethod( IsomorphicCopyByNormalSubloop, "for two loops",
    [ IsLoop, IsLoop ],
function( L, S )
    local p;
    if not IsNormal( L, S ) then 
        Error( "LOOPS: <2> must be a normal subloop of <1>");
    fi;
    # (PROG) SortingPerm is used rather than PermList since L is not necessarily canonical
    p := Inverse( SortingPerm( PosInParent( Concatenation( RightCosets( L, S ) ) ) ) );
    return IsomorphicCopyByPerm( L, p );
end);



#############################################################################
##  AUTOMORPHISMS AND AUTOMORPHISM GROUPS
##  -------------------------------------------------------------------------

#############################################################################
##  
#F  AutomorphismsFixingSet( S, L, GenL, DisL ) 
##
##  Auxiliary function. 
##  Given a loop <L>, its subset <S>, the efficient generators <GenL> of <L>
##  and the discriminator <DisL> of <L>, it returns all automorphisms of <L>
##  fixing the set <S> pointwise.

AutomorphismsFixingSet := function( S, L, GenL, DisL )
    local n, x, A, possible_images, y, i, map, g;
    
    # this is faster than extending a map
    n := Size( L );
    S := Subloop( L, List( S, i -> Elements( L )[ i ] ) );
    if Size( S ) = n then return []; fi; # identity, no need to return
    S := List( S, x -> Position( Elements( L ), x ) );
    
    #pruning blocks
    DisL := List( DisL, B -> Filtered( B, x -> not x in S ) );
    
    # first unmapped generator
    x := GenL[ 1 ]; 
    GenL := List( [2..Length( GenL )], i -> GenL[ i ] );
    
    A := [];
    
    possible_images :=  Difference( DisL[ SublistPosition( DisL, x ) ], [ x ] ); 
    for y in possible_images do
        # constructing map
        map := 0*[1..n];
        for i in [1..n] do if i in S then map[ i ] := i; fi; od;
        map[ x ] := y;
        g := [ map, Union( S, [ x ] ), Union( S, [ y ] ) ];
        # extending map
        g := ExtendIsomorphism( g, CayleyTable( L ), GenL, DisL, CayleyTable( L ), DisL );
        if not g = fail then AddSet( A, g[ 1 ] ); fi;
    od;
    
    S := Union( S, [ x ] );
    return Union( A, AutomorphismsFixingSet( S, L, GenL, DisL ) );  
end;

#############################################################################
##  
#F  AutomorphismGroup( L ) 
##
##  Returns the automorphism group of a loop <L>, as a permutation group
##  on [1..Size(L)].

InstallOtherMethod( AutomorphismGroup, "for loop",
    [ IsLoop ],
function( L )
    local DisL, GenL, A;
    # making sure L has canonical Cayley table
    if not L = Parent( L ) then L := LoopByCayleyTable( CayleyTable( L ) ); fi;
    DisL := Discriminator( L );
    GenL := List( EfficientGenerators( L, DisL ), x -> Position( Elements( L ), x ) );
    DisL := List( Discriminator( L )[ 2 ], B -> List( B, x -> Position( Elements( L ), x ) ) );
    
    A := AutomorphismsFixingSet( [ 1 ], L, GenL, DisL );
    
    if IsEmpty( A ) then return Group( () ); fi; # no notrivial automorphism
    return Group( List( A, p -> SortingPerm( p ) ) );
end);

#############################################################################
##  ISOMORPHISM TYPE OF MOUFANG LOOP
##  ------------------------------------------------------------------------

# (PROG) assumes that moufang_data, moufang_discriminators and 
# moufang_loops_by_discriminators are precalculated, but not necessarily bound

#############################################################################
##  
#O  IsomorphismTypeOfMoufangLoop( L ) 
##
##  Returns the isomorphism type of Moufang loop <L> in the form
##  [n,m], where n=Size(L) and m is the catalog number of <L> in the 
##  library of Moufang loops. See documentation for more details.

InstallMethod( IsomorphismTypeOfMoufangLoop, "for loop",
    [ IsLoop ],
function( L )
    local n, D, EG, p, data, M, iso;

    if not IsMoufangLoop( L ) then 
        Error("LOOPS: <1> must be a Moufang loop.");
    fi;
    if IsAssociative( L ) then 
        Error("LOOPS: <1> is a group.");
    fi;
    n := Size(L);
    if not n in [1..64] then
        Error("LOOPS: Isomorphism type is supported only for Moufang loops of order 1..64.");
    fi;
    
    # making sure that L has canonical Cayley table
    if not L = Parent( L ) then L := LoopByCayleyTable( CayleyTable( L ) ); fi;     
    D := Discriminator( L );
    EG := EfficientGenerators( L, D );
    if n in [1..64] then
        if IsEmpty(moufang_discriminators) then
            ReadPkg( "loops", "data/moufang_discriminators.tbl");
        fi;
    
        p := Position( moufang_discriminators, D[1] );
        if not p = fail then
            for data in moufang_loops_by_discriminators[ p ] do
                M := MoufangLoop( data[1], data[2] );
                iso := IsomorphismLoopsNC( L, EG, D, M, Discriminator( M ) );
                if not iso = fail then return [data, iso]; fi;
            od;
        fi;
    fi;
end);

#############################################################################
##  ISOTOPISM OF LOOPS
##  ------------------------------------------------------------------------

#############################################################################
##  
#O  IsotopismLoops( L1, L2 ) 
##
##  If L1, L2 are isotopic loops, returns true, else fail.

# (MATH) First we calculate all principal loop isotopes of L1 of the form
# PrincipalLoopIsotope(L1, f, g), where f, g, are elements of L1. 
# Then we filter these up to isomorphism. If L2 is isotopic to L1, then
# L2 is isomorphic to one of these principal isotopes.

InstallMethod( IsotopismLoops, "for two loops",
    [ IsLoop, IsLoop ],
function( L1, L2 )
    local istps, fg, f, g, L, phi, pos, alpha, beta, gamma, p;
    
    # make all loops canonical to be able to calculate isotopisms
    if not L1 = Parent( L1 ) then L1 := LoopByCayleyTable( CayleyTable( L1 ) ); fi;
    if not L2 = Parent( L2 ) then L2 := LoopByCayleyTable( CayleyTable( L2 ) ); fi;
    
    # first testing for isotopic invariants
    if not Size(L1)=Size(L2) then return fail; fi;
    if IsomorphismLoops( Center(L1), Center(L2) ) = fail then return fail; fi;
    if IsomorphismLoops( LeftNucleus(L1), LeftNucleus(L2) ) = fail then return fail; fi;
    if IsomorphismLoops( RightNucleus(L1), RightNucleus(L2) ) = fail then return fail; fi;
    if IsomorphismLoops( MiddleNucleus(L1), MiddleNucleus(L2) ) = fail then return fail; fi;
    # we could test for isomorphism among multiplication groups and inner mapping group, too
    if not Size(MultiplicationGroup(L1)) = Size(MultiplicationGroup(L2)) then return fail; fi;
    if not Size(InnerMappingGroup(L1)) = Size(InnerMappingGroup(L2)) then return fail; fi;
    
    # now trying to construct an isotopism
    istps := [];
    fg := [];
    for f in L1 do for g in L1 do 
        Add(istps, PrincipalLoopIsotope( L1, f, g ));
        Add(fg, [ f, g ] );
    od; od;
    for L in LoopsUpToIsomorphism( istps ) do 
        phi := IsomorphismLoops( L, L2 );
        if not phi = fail then 
            # must reconstruct the isotopism (alpha, beta, gamma)
            # first figure out what f and g were
            pos := Position( istps, L );
            f := fg[ pos ][ 1 ]; 
            g := fg[ pos ][ 2 ];
            alpha := Inverse( RightTranslation( L1, g ) );
            beta := Inverse( LeftTranslation( L1, f ) );
            # we also applied an isomorphism (1,f*g) inside PrincipalLoopIsotope           
            p := Position( L1, f*g );
            gamma := ();
            if p > 1 then
                alpha := alpha * (1,p);
                beta := beta * (1,p);
                gamma := gamma * (1,p);
            fi;
            # finally, we apply the isomorphism phi
            alpha := alpha * phi;
            beta := beta * phi;
            gamma := gamma * phi;
            return [ alpha, beta, gamma ];
        fi; 
    od;            
    return fail;
end);


#############################################################################
##  
#O  LoopsUpToIsotopism( ls ) 
##
##  Given a list <ls> of loops, returns a sublist of <ls> consisting
##  of represenatives of isotopism classes of <ls>. VERY SLOW!

InstallMethod( LoopsUpToIsotopism, "for a list of loops",
    [ IsList ],
function( ls )
    local loops, L, is_new_loop, K, M, istps, f, g;
    # making sure only loops are on the list
    if not IsEmpty( Filtered( ls, x -> not IsLoop( x ) ) ) then
        Error("<arg1> must be a list of loops");
    fi;        
    loops := [];
    for L in ls do
        is_new_loop := true;
        # find all principal isotopes of L up to isomorphism
        istps := [];
        for f in L do for g in L do 
            Add(istps, PrincipalLoopIsotope( L, f, g ) );
        od; od;
        istps := LoopsUpToIsomorphism(istps);
        # check if any is isomorphic to a found loop        
        for K in loops do for M in istps do
            if not IsomorphismLoops( K, M ) = fail then 
                is_new_loop := false; break;
            fi; 
        od; od;
        if is_new_loop then Add( loops, L ); fi;
    od;
    return loops;
end);
