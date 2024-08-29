#############################################################################
##
#W  iso.gi  Isomorphisms and isotopisms [loops]
##  
##  
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  DISCRIMINATOR
##  -------------------------------------------------------------------------

#############################################################################
##  
#O  Discriminator( Q ) 
##    
##  Returns the discriminator of a quasigroup <Q>.
##  It is a list [ A, B ], where A is a list of the form
##  [ [I1,n1], [I2,n2],.. ], where the invariant Ii occurs ni times in Q,
##  and where B[i] is a subset of [1..Size(Q)] corresponding to elements of
##  Q with invariant Ii.
##  
##  PROG: Invariants 8, 9 can be slow for large quasigroups.

InstallMethod( Discriminator, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local n, T, I, case, i, j, k, ebo, js, ks, count1, count2, A, P, B, perm, p;

    # making sure the quasigroup is canonical
    if not Q = Parent( Q ) then Q := CanonicalCopy( Q ); fi;
    n := Size( Q );
    
    # converting a non-declared loop into a loop
    perm := ();
    if (not IsLoop( Q )) and (not MultiplicativeNeutralElement( Q )=fail) then
        p := Position( Elements( Q ), MultiplicativeNeutralElement( Q ) );
        if p<>1 then
            perm := (1,p);
        fi;
        Q := IntoLoop( Q );
    fi;
    
    T := CayleyTable( Q );
           
    # Calculating 9 invariants for three cases: quasigroup, loop, power associative loop
    # I[i] will contain the invariant vector for ith element of Q
    I := List( [1..n], i -> 0*[1..9] );

    # invariant 1
    # to distinguish the 3 cases
    if (not IsLoop( Q ) )
        then case := 1;
    elif (not IsPowerAssociative( Q ))
        then case := 2;
    else
        case := 3;
    fi;
    for i in [1..n] do I[i][1] := case; od;
    
    # invariant 2
    # for given x, cycle structure of L_x, R_x
    for i in [1..n] do
        I[i][2] := [
            CycleStructurePerm( PermList( List( [1..n], j -> T[i][j]) ) ),
            CycleStructurePerm( PermList( List( [1..n], j -> T[j][i]) ) )
        ];
    od;

    # invariant 3
    if case = 1 then # am I an idempotent?
        for i in [1..n] do I[i][3] := T[i][i]=i; od;
    elif case = 2 then # am I an involution?
        for i in [1..n] do I[i][3] := T[i][i]=1; od;
    else # what's my order?
        for i in [1..n] do I[i][3] := Order( Elements( Q )[i] ); od;
    fi;

    # invariant 4
    if case <> 3 then # how many times am I a square ?
        for i in [1..n] do j := T[i][i]; I[j][4] := I[j][4] + 1; od;
    else # how many times am I a square, third power, fourth power?
        for i in [1..n] do I[i][4] := [0,0,0]; od;
        for i in [1..n] do
            j := T[i][i]; I[j][4][1] := I[j][4][1] + 1;
            j := T[i][j]; I[j][4][2] := I[j][4][2] + 1;
            j := T[i][j]; I[j][4][3] := I[j][4][3] + 1;
        od;
    fi;
    
    # invariant 5
    if case <> 3 then #  with how many elements do I commute?
        for i in [1..n] do
            I[i][5] := Length( Filtered( [1..n], j -> T[i][j] = T[j][i] ) );
        od;
    else # with how many elements of given order do I commute?
        ebo := List( [1..n], i -> Filtered( [1..n], j -> I[j][3]=i ) ); # elements by order. PROG: must point to order invariant
        ebo := Filtered( ebo, x -> not IsEmpty( x ) );
        for i in [1..n] do
            I[i][5] := List( ebo, J -> Length( Filtered( J, j -> T[ i ][ j ] = T[ j ][ i ] ) ) );
        od;
    fi;

    # invariant 6
    # is it true that (x*x)*x = x*(x*x)?
    for i in [1..n] do
        I[i][6] :=  T[T[i][i]][i] = T[i][T[i][i]];
    od;

    # invariant 7
    if case <> 3 then # for how many elements y is (x*x)*y = x*(x*y)?
        for i in [1..n] do
            I[i][7] := Length( Filtered( [1..n], j -> T[T[i][i]][j] = T[i][T[i][j]] ) );
        od;
    else # for how many elements y of given order is (x*x)*y=x*(x*y)
        for i in [1..n] do
            I[i][7] := List( ebo, J -> Length( Filtered( J, j -> T[T[i][i]][j] = T[i][T[j][i]] ) ) );
        od;
    fi;

    # invariants 8 and 9 (these take longer)
    if case <> 3 then # with how many pairs of elements do I associate in the first, second position?
        for i in [1..n] do
            for j in [1..n] do for k in [1..n] do
                if T[i][T[j][k]] = T[T[i][j]][k] then I[i][8] := I[i][8] + 1; fi;
                if T[j][T[i][k]] = T[T[j][i]][k] then I[i][9] := I[i][9] + 1; fi;
            od; od;
        od;
    else # for how many pairs of elements of given orders do I associate in the first, second position?
        for i in [1..n] do
            I[i][8] := []; I[i][9] := [];
            for js in ebo do for ks in ebo do
                count1 := 0; count2 := 0;
                for j in js do for k in ks do
                    if T[i][T[j][k]] = T[T[i][j]][k] then count1 := count1 + 1; fi;
                    if T[j][T[i][k]] = T[T[j][i]][k] then count2 := count2 + 1; fi;
                od; od;
                Add( I[i][8], count1 ); Add( I[i][9], count2 );
            od; od;
        od;   
    fi;
        	
    # all invariants have now been calculated
    
    # note that it can be deduced from the invariants when an element is central, for instance 

    # setting up the first part of the discriminator (invariants with the number of occurrence)
    A := Collected( I );
    P := Sortex( List( A, x -> x[2] ) ); # rare invariants will be listed first, but the set ordering of A is otherwise not disrupted
    A := Permuted( A, P );

    # setting up the second part of the discriminator (blocks of elements invariant under isomorphisms)
    B := List( [1..Length(A)], j -> Filtered( [1..n], i -> I[i] = A[j][1] ) );
    
    # if a non-declared loop was converted into a loop, correcting for this
    if not perm = () then
        B := List( B, x -> Set( x, i -> i^perm ) );
    fi;
    
    return [ A, B ];

end);

#############################################################################
##  
#F  LOOPS_EfficientGenerators( Q, D ) 
##    
##  Auxiliary function. 
##  Given a quasigroup <Q> with discriminator <D>, it returns a list of
##  indices of generators of <Q> deemed best for an isomorphism filter.
##  It mimics the function SmallGeneratingSet, but it considers
##  the elements in order determined by block size of the discriminator.

InstallGlobalFunction( LOOPS_EfficientGenerators,
function( Q, D ) 
    local gens, sub, elements, candidates, max, S, best_gen, best_S;

    gens := [];                             # generating set to be returned
    sub := [];                              # substructure generated so far
    elements := Concatenation( D[2] );      # all elements ordered by block size
    candidates := ShallowCopy( elements );  # candidates for next generator
    while sub <> Q do
        # find an element not in sub that most enlarges sub
        max := 0;
        while not IsEmpty( candidates ) do
            S := Subquasigroup( Q, Union( gens, [candidates[1]] ) );
            if Size(S) > max then
                max := Size( S );
                best_gen := candidates[1];
                best_S := S;
            fi;
            # discard elements of S since they cannot do better
            candidates := Filtered( candidates, x -> not Elements(Q)[x] in S );
        od;
        Add( gens, best_gen );
        sub := best_S;
        # reset candidates for next round
        candidates := Filtered( elements, x -> not Elements(Q)[x] in sub );
    od;
    return gens;

end);

#############################################################################
##  
#O  AreEqualDiscriminators( D, E ) 
##    
##  Returns true if the invariants of the two discriminators are the same,
##  including number of occurrences of each invariant.
  
InstallMethod( AreEqualDiscriminators, "for two lists (discriminators)",
    [ IsList, IsList ],
function( D, E )
      return D[ 1 ] = E[ 1 ];
end);

#############################################################################
##  EXTENDING MAPPINS (AUXILIARY)
##  -------------------------------------------------------------------------

# Here, we identity the map f: A --> B  with the triple [ m, a, b ], 
# where a is a subset of A, b[ i ] is the image of a[ i ], and m[ i ] > 0
# if and only if i is in a.

#############################################################################
##  
#F  LOOPS_ExtendHomomorphismByClosingSource( f, L, M ) 
##
##  Auxiliary.    
##  <L>, <M> are multiplication tables of quasigroups, <f> is a partial map
##  from a subset of elements of <L> to a subset of elements of <M>. 
##  This function attempts to extend <f> into a homomorphism of quasigroups by 
##  extending the source of <f> into (the smallest possible) subquasigroup of <L>.

InstallGlobalFunction( LOOPS_ExtendHomomorphismByClosingSource,
function( f, L, M )
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
end);

#############################################################################
##  
#F  LOOPS_SublistPosition( S, x ) 
##  
##  auxiliary function  
##  input: list of lists <S>, element <x>
##  returns: smallest i such that x in S[i]; or fail.

InstallGlobalFunction( LOOPS_SublistPosition,
function( S, x )
    local i;
    for i in [ 1..Length( S ) ] do if x in S[ i ] then return i; fi; od;
    return fail;
end);

#############################################################################
##  
#F  LOOPS_ExtendIsomorphism( f, L, GenL, DisL, M, DisM ) 
##  
##  Auxiliary.  
##  Given a partial map <f> from a quasigroup <L> to a quasigroup <M>,
##  it attempts to extend <f> into an isomorphism between <L> and <M>.
##  <GenL>, <DisL> and <DisM> are precalculated and stand for:
##  efficient generators of <L>, invariant subsets of <L>, efficient generators
##  of <M>, respectively.

InstallGlobalFunction( LOOPS_ExtendIsomorphism,
function( f, L, GenL, DisL, M, DisM )
    local x, possible_images, y, g;
    f := LOOPS_ExtendHomomorphismByClosingSource( f, L, M );
    if f = fail or Length( f[ 2 ] ) > Length( f[ 3 ] ) then return fail; fi;
    if Length( f[ 2 ] ) = Length( L ) then return f; fi; #isomorphism found
    
    x := GenL[ 1 ];
    GenL := GenL{[2..Length(GenL)]}; 
    possible_images := Filtered( DisM[ LOOPS_SublistPosition( DisL, x ) ], y -> not y in f[ 3 ] );    
    for y in possible_images do
        g := StructuralCopy( f );
        g[ 1 ][ x ] := y; AddSet( g[ 2 ], x ); AddSet( g[ 3 ], y );
        g := LOOPS_ExtendIsomorphism( g, L, GenL, DisL, M, DisM );
        if not g = fail then return g; fi; #isomorphism found
    od;
    return fail;    
end);

#############################################################################
##  ISOMORPHISMS OF LOOPS
##  -------------------------------------------------------------------------

#############################################################################
##  
#O  IsomorphismQuasigroupsNC( L, GenL, DisL, M, DisM ) 
##    
##  Auxiliary. Given a quasigroup <L>, its efficient generators <GenL>, the 
##  discriminator <DisL> of <L>, and another loop <M> with discriminator
##  <DisM>, it returns an isomorphism from <L> onto <M>, or it fails.

InstallGlobalFunction( IsomorphismQuasigroupsNC,
function( L, GenL, DisL, M, DisM )
    local map, iso;
    if not AreEqualDiscriminators( DisL, DisM ) then return fail; fi;

    #mapping
    map := 0 * [ 1.. Size( L ) ]; 
    
    if IsLoop( L ) and IsLoop( M ) then
        map[ 1 ] := 1; # identity element is certainly preserved
        iso := LOOPS_ExtendIsomorphism( [ map, [ 1 ], [ 1 ] ], CayleyTable( L ), GenL, DisL[2], CayleyTable( M ), DisM[2] ); 
    else
        iso := LOOPS_ExtendIsomorphism( [ map, [ ], [ ] ], CayleyTable( L ), GenL, DisL[2], CayleyTable( M ), DisM[2] );
    fi;
    if not iso = fail then return SortingPerm( iso[ 1 ] ); fi;
    return fail;
end);

#############################################################################
##  
#O  IsomorphismQuasigroups( L, M ) 
##
##  If the quasigroups <L>, <M> are isomorphic, it returns an isomorphism
##  from <L> onto <M>. Fails otherwise.

InstallMethod( IsomorphismQuasigroups, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( L, M )
    local GenL1, GenL2, GenL, DisL, DisM, permL, permM, p, iso; 
   
    # making sure the quasigroups have canonical Cayley tables
    if not L = Parent( L ) then L := CanonicalCopy( L ); fi;
    if not M = Parent( M ) then M := CanonicalCopy( M ); fi;
    
    # turning non-declared loops into loops
    permL := ();
    if (not IsLoop( L )) and (not MultiplicativeNeutralElement( L )=fail) then
        p := Position( Elements( L ), MultiplicativeNeutralElement( L ) );
        if p<>1 then
            permL := (1,p);
        fi;
        L := IntoLoop( L );
    fi;
    permM := ();
    if (not IsLoop( M )) and (not MultiplicativeNeutralElement( M )=fail) then
        p := Position( Elements( M ), MultiplicativeNeutralElement( M ) );
        if p<>1 then
            permM := (1,p);
        fi;
        M := IntoLoop( M );
    fi;
   
    DisL := Discriminator( L );
    GenL := LOOPS_EfficientGenerators( L, DisL );
    DisM := Discriminator( M );
    iso := IsomorphismQuasigroupsNC( L, GenL, DisL, M, DisM );
    if not iso = fail then
        iso := permL*iso*permM; # accounting for possible internal conversions to loops
    fi;
    return iso;
end);

#############################################################################
##  
#O  IsomorphismLoops( L, M ) 
##
##  If the loops <L>, <M> are isomorphic, it returns an isomorphism
##  from <L> onto <M>. Fails otherwise.

InstallMethod( IsomorphismLoops, "for loops",
    [ IsLoop, IsLoop ],
function( L, M )
    return IsomorphismQuasigroups( L, M );
end);

#############################################################################
##  
#O  QuasigroupsUpToIsomorphism( ls ) 
##
##  Given a list <ls> of quasigroups, returns a sublist of <ls> consisting
##  of representatives of isomorphism classes of <ls>.

InstallMethod( QuasigroupsUpToIsomorphism, "for a list of quasigroups",
    [ IsList ],
function( ls )
    local i, quasigroups, L, D, G, with_same_D, is_new_quasigroup, K;
    
    # making sure only quasigroups are on the list
    if not IsEmpty( Filtered( ls, x -> not IsQuasigroup( x ) ) ) then
        Error("LOOPS: <1> must be a list of quasigroups");
    fi;        
    # special case: one quasigroup
    if Length( ls ) = 1 then
        return ls;
    fi;
    # making everything canonical
    ls := ShallowCopy( ls ); # otherwise a side effect occurs in ls
    for i in [1..Length(ls)] do
        if not Parent(ls[i]) = ls[i] then
            ls[i] := CanonicalCopy( ls[i] );
        fi;
    od;
    
    quasigroups := [];
    for L in ls do
        D := Discriminator( L );
        G := LOOPS_EfficientGenerators( L, D );
        # will be testing only quasigroups with the same discriminator
        with_same_D := Filtered( quasigroups, K -> AreEqualDiscriminators( K[2], D ) );
        is_new_quasigroup := true;
        for K in with_same_D do
            if not IsomorphismQuasigroupsNC( L, G, D, K[1], K[2] ) = fail then
                is_new_quasigroup := false;
                break;
            fi;
        od;
        if is_new_quasigroup then Add( quasigroups, [ L, D ] ); fi;
    od;
    # returning only quasigroups, not their discriminators
    return List( quasigroups, L -> L[1] );
end);

#############################################################################
##  
#O  LoopsUpToIsomorphism( ls ) 
##
##  Given a list <ls> of loops, returns a sublist of <ls> consisting
##  of representatives of isomorphism classes of <ls>.

InstallMethod( LoopsUpToIsomorphism, "for a list of loops",
    [ IsList ],
function( ls )
    # making sure only quasigroups are on the list
    if not IsEmpty( Filtered( ls, x -> not IsQuasigroup( x ) ) ) then
        Error("LOOPS: <1> must be a list of quasigroups");
    fi;        
    return QuasigroupsUpToIsomorphism( ls );
end);

#############################################################################
##  
#O  QuasigroupIsomorph( Q, p ) 
##
##  If <Q> is a quasigroup of order n and <p> a permutation of [1..n], returns
##  the quasigroup (Q,*) such that p(xy) = p(x)*p(y).

InstallMethod( QuasigroupIsomorph, "for a quasigroup and permutation",
    [ IsQuasigroup, IsPerm ],
function( Q, p )
    local ctQ, ct, inv_p;
    ctQ := CanonicalCayleyTable( CayleyTable( Q ) );
    inv_p := Inverse( p );
    ct := List([1..Size(Q)], i-> List([1..Size(Q)], j -> 
        ( ctQ[ i^inv_p ][ j^inv_p ] )^p 
    ) );
    return QuasigroupByCayleyTable( ct );
end);

#############################################################################
##  
#O  LoopIsomorph( Q, p ) 
##
##  If <Q> is a loop of order n and <p> a permutation of [1..n] such that
##  p(1)=1, returns the loop (Q,*) such that p(xy)=p(x)*p(y).
##  If p(1)=c<>1, then the quasigroup (Q,*) is converted into loop
##  via the isomorphism (1,c).

InstallMethod( LoopIsomorph, "for a loop and permutation",
    [ IsLoop, IsPerm ],
function( Q, p )
    return IntoLoop( QuasigroupIsomorph( Q, p ) );
end);

#############################################################################
##  
#O  IsomorphicCopyByPerm( Q, p ) 
##
##  Calls LoopIsomorph( Q, p ) if <Q> is a loop,
##  else QuasigroupIsotope( Q, p ).

InstallMethod( IsomorphicCopyByPerm, "for a quasigroup and permutation",
    [ IsQuasigroup, IsPerm ],
function( Q, p )
    if IsLoop( Q ) then 
        return LoopIsomorph( Q, p );
    fi;
    return QuasigroupIsomorph( Q, p );
end);

#############################################################################
##  
#O  IsomorphicCopyByNormalSubloop( L, S ) 
##
##  Given a loop <L> and its normal subloop <S>, it returns an isomorphic 
##  copy of <L> with elements reordered according to right cosets of S

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
#F  LOOPS_AutomorphismsFixingSet( S, Q, GenQ, DisQ ) 
##
##  Auxiliary function. 
##  Given a quasigroup <Q>, its subset <S>, the efficient generators
##  <GenQ> of <Q> and and invariant subsets <DisQ> of <Q>, it returns all
##  automorphisms of <Q> fixing the set <S> pointwise.

InstallGlobalFunction( LOOPS_AutomorphismsFixingSet,
function( S, Q, GenQ, DisQ )
    local n, x, A, possible_images, y, i, map, g;
    
    # this is faster than extending a map
    n := Size( Q );
    S := Subquasigroup( Q, S ); # can be empty if Q is not a loop
    if Size( S ) = n then return []; fi; # identity, no need to return
    S := List( S, x -> Position( Elements( Q ), x ) );
    
    #pruning blocks
    DisQ := List( DisQ, B -> Filtered( B, x -> not x in S ) );
    
    # first unmapped generator
    x := GenQ[ 1 ]; 
    GenQ := GenQ{[2..Length(GenQ)]};
    
    A := [];
    
    possible_images := Filtered( DisQ[ LOOPS_SublistPosition( DisQ, x ) ], y -> y <> x );   
    for y in possible_images do
        # constructing map
        map := 0*[1..n];
        for i in [1..n] do if i in S then map[ i ] := i; fi; od;
        map[ x ] := y;
        g := [ map, Union( S, [ x ] ), Union( S, [ y ] ) ];
        # extending map
        g := LOOPS_ExtendIsomorphism( g, CayleyTable( Q ), GenQ, DisQ, CayleyTable( Q ), DisQ );
        if not g = fail then AddSet( A, g[ 1 ] ); fi;
    od;
    
    S := Union( S, [ x ] );
    return Union( A, LOOPS_AutomorphismsFixingSet( S, Q, GenQ, DisQ ) );  
end);

#############################################################################
##  
#F  AutomorphismGroup( Q ) 
##
##  Returns the automorphism group of a quasigroup <Q>,
##  as a permutation group on [1..Size(Q)].

InstallOtherMethod( AutomorphismGroup, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local DisQ, GenQ, A;
    # making sure Q has canonical Cayley table
    if not Q = Parent( Q ) then Q := CanonicalCopy( Q ); fi;
    DisQ := Discriminator( Q );
    GenQ := LOOPS_EfficientGenerators( Q, DisQ );
        
    if IsLoop( Q ) then
        A := LOOPS_AutomorphismsFixingSet( [ 1 ], Q, GenQ, DisQ[2] );
    else
        A := LOOPS_AutomorphismsFixingSet( [ ], Q, GenQ, DisQ[2] );
    fi;
    
    if IsEmpty( A ) then return Group( () ); fi; # no nontrivial automorphism
    return Group( List( A, p -> SortingPerm( p ) ) );
end);

#############################################################################
##  ISOTOPISM OF LOOPS
##  ------------------------------------------------------------------------

#############################################################################
##  
#O  IsotopismLoops( L1, L2 ) 
##
##  If L1, L2 are isotopic loops, returns true, else fail.

# (MATH) We check for isomorphism of L2 against all principal
# isotopes of L1.

InstallMethod( IsotopismLoops, "for two loops",
    [ IsLoop, IsLoop ],
function( L1, L2 )
    local f, g, L, phi, alpha, beta, gamma, p;
    
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
    for f in L1 do for g in L1 do 
        L := PrincipalLoopIsotope( L1, f, g );
        phi := IsomorphismLoops( L, L2 );
        if not phi = fail then 
            # must reconstruct the isotopism (alpha, beta, gamma)
            alpha := RightTranslation( L1, g );
            beta := LeftTranslation( L1, f );
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
    od; od;           
    return fail;
end);


#############################################################################
##  
#O  LoopsUpToIsotopism( ls ) 
##
##  Given a list <ls> of loops, returns a sublist of <ls> consisting
##  of representatives of isotopism classes of <ls>. VERY SLOW!

InstallMethod( LoopsUpToIsotopism, "for a list of loops",
    [ IsList ],
function( ls )
    local loops, L, is_new_loop, K, M, istps, f, g;
    # making sure only loops are on the list
    if not IsEmpty( Filtered( ls, x -> not IsLoop( x ) ) ) then
        Error("LOOPS: <1> must be a list of loops");
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
