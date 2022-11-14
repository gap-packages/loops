#############################################################################
##
#W  core_methods.gi Most common structural methods [loops]
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  GENERATORS
##  -------------------------------------------------------------------------

#############################################################################
##
#A  GeneratorsOfQuasigroup( Q )
##
##  Returns a subset of <Q> generating <Q>. In most circumstances,
##  Elements( Q ) is returned.

InstallMethod( GeneratorsOfQuasigroup, "for quasigroup",
    [ IsQuasigroup ],
    GeneratorsOfDomain
);

#############################################################################
##
#A  GeneratorsSmallest( Q )
##
##  Returns a smallest generating set of a quasigroup <Q> with respect
##  to lexicographic ordering of elements.

InstallOtherMethod( GeneratorsSmallest, "for a quasigroup",
        [ IsQuasigroup ],
function( Q )
    local gens, diff;
    gens := [ ];
    diff := Elements( Q );
    while diff <> [ ] do
        Add( gens, diff[ Length(diff) ] );
        diff := Difference( diff, Subquasigroup( Q, gens ) );
    od;
    return Set( gens );
end );

#############################################################################
##
#A  SmallGeneratingSet( Q )
##
##  Returns a small generating set of a quasigroup <Q>. There is no
##  guarantee that the set is of minimal cardinality.

InstallOtherMethod( SmallGeneratingSet, "for a quasigroup",
        [ IsQuasigroup ],
function( Q )
    local gens, sub, candidates, max, S, best_gen, best_S;
    
    gens := [];         # generating set to be returned
    sub := [];          # substructure generated so far
    while sub <> Q do
        # find an element not in sub that most enlarges sub
        candidates := Difference( Elements(Q), sub );
        max := 0;
        while not IsEmpty( candidates ) do
            S := Subquasigroup( Q, Union( gens, [candidates[1]] ) );
            if Size(S) > max then
                max := Size( S );
                best_gen := candidates[1];
                best_S := S;
            fi;
            # discard elements of S since they cannot do better
            candidates := Difference( candidates, Elements( S ) );
        od;
        AddSet( gens, best_gen );
        sub := best_S;
    od;
    return gens;
end );

#############################################################################
##  COMPARING QUASIGROUPS WITH COMMON PARENT
##  -------------------------------------------------------------------------

#############################################################################
##
#A  \<( L, K )
##
##  Ordering between quasigroups with common parent quasigroup. Analogous to the
##  basic method for groups. We use the lexicographical ordering between the
##  small (greedy) generating sets.

InstallMethod(\<,
    "for quasigroups by lexicographically ordered small generating sets",
    IsIdenticalObj,
    [IsQuasigroup, IsQuasigroup],
function(a,b)
    local ga,gb;
    ga:=GeneratorsSmallest(a);
    gb:=GeneratorsSmallest(b);
    if Length(ga)<Length(gb) then
        a:=Elements(a)[Size(a)];
        ga:=ShallowCopy(ga);
        while Length(ga)<Length(gb) do Add(ga,a); od;
    else
        b:=Elements(b)[Size(b)];
        gb:=ShallowCopy(gb);
        while Length(gb)<Length(ga) do Add(gb,b); od;
    fi;
    return ga<gb;
end);


#############################################################################
##  SECTIONS
##  -------------------------------------------------------------------------

#############################################################################
##
#A  LeftSection( Q )
##
##  Returns the left section of a quasigroup <Q>. Left section consists of
##  all left translations L_x: y --> x*y.

InstallMethod( LeftSection, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local P;
    P := PosInParent( Q );
    return List( CayleyTable( Q ), row -> MappingPermListList( P, row ) );
end );

#############################################################################
##
#A  RightSection( Q )
##
##  Returns the right section of a quasigroup <Q>. Right section consists of
##  all right translations R_x: y --> y*x.

InstallMethod( RightSection, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local P;
    P := PosInParent( Q );
    return List( TransposedMat( CayleyTable( Q ) ), row -> MappingPermListList( P, row ) );
end );

#############################################################################
##
#O  LeftTranslation( Q, x )
##
##  Let <x> be an element of a quasigroup <Q>. Returns the left translation
##  by <x> in <Q>.

InstallMethod( LeftTranslation, "for quasigroup and quasigroup element",
    [ IsQuasigroup, IsQuasigroupElement ],
function( Q, x )
    if not x in Q then
        Error( "LOOPS: <2> must be an element of the quasigroup <1>.");
    fi;
    return LeftSection( Q )[ Position( Q, x ) ];
end);

#############################################################################
##
#O  RightTranslation( Q, x )
##
##  Let <x> be an element of a quasigroup <Q>. Returns the right translation
##  by <x> in <Q>.

InstallMethod( RightTranslation, "for quasigroup and quasigroup element",
    [ IsQuasigroup, IsQuasigroupElement ],
function( Q, x )
    if not x in Q then
        Error( "LOOPS: <2> must be an element of the quasigroup <1>.");
    fi;
    return RightSection( Q )[ Position( Q, x ) ];
end);

#############################################################################
##  MULTIPLICATION GROUPS
##  -------------------------------------------------------------------------

#############################################################################
##
#A  LeftMultiplicationGroup( Q )
##
##  Returns the subgroup of S_<Q> generated by all left translations of the
##  quasigroup <Q>.

InstallMethod( LeftMultiplicationGroup, "for quasigroup",
    [IsQuasigroup ],
function( Q )
    return Group( LeftSection( Q ) );
end );

#############################################################################
##
#A  RightMultiplicationGroup( Q )
##
##  Returns the subgroup of S_<Q> generated by all right translations of the
##  quasigroup <Q>.

InstallMethod( RightMultiplicationGroup, "for quasigroup",
    [IsQuasigroup ],
function( Q )
    return Group( RightSection( Q ) );
end );

#############################################################################
##
#A  MultiplicationGroup( Q )
##
##  Returns the smallest subgroup of S_<Q> containing the both the left
##  and right multiplication groups of the quasigroup <Q>.

InstallMethod( MultiplicationGroup, "for quasigroup",
    [IsQuasigroup ],
function( Q )
    return Group( Union( LeftSection( Q ), RightSection( Q ) ) );
end );

#############################################################################
##
#O  RelativeLeftMultiplicationGroup( L, S )
##
##  Returns the relative left multiplication group of a quasigroup <L>
##  with respect to its subquasigroup <S>.

InstallMethod( RelativeLeftMultiplicationGroup, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( L, S )
    local perms;
    if not IsSubquasigroup( L, S ) then
        Error("LOOPS: <2> must be a subquasigroup of <1>.");
    fi;
    perms := List( S, x -> LeftTranslation( L, x ) );
    return Group( perms );
end);

#############################################################################
##
#O  RelativeRightMultiplicationGroup( L, S )
##
##  Returns the relative right multiplication group of a quasigroup <L>
##  with respect to its subquasigroup <S>.

InstallMethod( RelativeRightMultiplicationGroup, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( L, S )
    local perms;
    if not IsSubquasigroup( L, S ) then
        Error("LOOPS: <2> must be a subquasigroup of <1>.");
    fi;
    perms := List( S, x -> RightTranslation( L, x ) );
    return Group( perms );
end);

#############################################################################
##
#O  RelativeMultiplicationGroup( L, S )
##
##  Returns the relative multiplication group of a quasigroup <L>
##  with respect to its subquasigroup <S>.

InstallMethod( RelativeMultiplicationGroup, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( L, S )
    return Group( Union(
        RelativeLeftMultiplicationGroup( L, S ),
        RelativeRightMultiplicationGroup( L, S)
    ) );
end);

#############################################################################
##  INNER MAPPING GROUPS
##  -------------------------------------------------------------------------

#############################################################################
##
#O  LeftInnerMapping( L, x, y )
##
##  Returns the left inner mapping of loop <L> determined by elements
##  <x>, <y>, i.e., the mapping L(x,y) = L_{yx}^{-1} L_y L_x, where we compose
##  mappings from right to left.

InstallMethod( LeftInnerMapping, "for loop and a loop elements",
    [ IsLoop, IsLoopElement, IsLoopElement ],
function( L, x, y )
    if not (x in L and y in L) then
        Error( "LOOPS: <2>, <3> must be elements of the loop <1>.");
    fi;
    return LeftTranslation( L, x ) * LeftTranslation( L, y ) * LeftTranslation( L, y*x )^(-1);
end);

#############################################################################
##
#O  MiddleInnerMapping( L, x )
##
##  Let <x> be an element of a loop <L>. Returns the middle inner mapping
##  determined by <x>, i.e., the mapping L(x)^{-1} R(x), where we compose
##  mappings from right to left.

InstallMethod( MiddleInnerMapping, "for loop and a loop elements",
    [ IsLoop, IsLoopElement ],
function( L, x )
    if not x in L then
        Error( "LOOPS: <2> must be an element of the loop <1>.");
    fi;
    return RightTranslation( L, x ) * LeftTranslation( L, x )^(-1);
end);

#############################################################################
##
#O  RightInnerMapping( L, x, y )
##
##  Returns the right inner mapping of loop <L> determined by elements
##  <x>, <y>, i.e., the mapping R(x,y) = R_{xy}^{-1} R_y R_x, where we compose
##  mappings from right to left.

InstallMethod( RightInnerMapping, "for loop and a loop elements",
    [ IsLoop, IsLoopElement, IsLoopElement ],
function( L, x, y )
    if not (x in L and y in L) then
        Error( "LOOPS: <2>, <3> must be elements of the loop <1>.");
    fi;
    return RightTranslation( L, x ) * RightTranslation( L, y ) * RightTranslation( L, x*y )^(-1);
end);

#############################################################################
##
#A  InnerMappingGroup( L )
##
##  Returns the inner mapping group of a loop <L>, i.e., the subgroup
##  of MultplicationGroup( L ) consisting of maps fixing One( L ).

InstallMethod( InnerMappingGroup, "for loop",
    [ IsLoop ],
function( L )
    return Stabilizer( MultiplicationGroup( L ), 1 );
end);

#############################################################################
##
#A  LeftInnerMappingGroup( L )
##
##  Returns the left inner mapping group of a loop <L>, i.e., the subgroup
##  of LeftMultiplicationGroup( L ) consisting of maps fixing One( L ).

InstallMethod( LeftInnerMappingGroup, "for loop",
    [ IsLoop ],
function( L )
    return Stabilizer( LeftMultiplicationGroup( L ), 1 );
end);

#############################################################################
##
#A  MiddleInnerMappingGroup( L )
##
##  returns the middle inner mapping group of a loop <L>

InstallMethod( MiddleInnerMappingGroup, "for loop",
   [ IsLoop ],
function( L )
   return Group( List( L, x -> MiddleInnerMapping( L, x ) ) );
end);

#############################################################################
##
#A  RightInnerMappingGroup( L )
##
##  Returns the right inner mapping group of a loop <L>, i.e., the subgroup
##  of RightMultiplicationGroup( L ) consisting of maps fixing One( L ).

InstallMethod( RightInnerMappingGroup, "for loop",
    [ IsLoop ],
function( L )
    return Stabilizer( RightMultiplicationGroup( L ), 1 );
end);

#############################################################################
##  SUBQUASIGROUPS AND SUBLOOPS
##  -------------------------------------------------------------------------

InstallOtherMethod( Position, "for quasigroup and quasigroup element",
    [ IsQuasigroup, IsQuasigroupElement, IsInt],
function( Q, x, dummy )
    return Position( Elements( Q ), x );
end );

#############################################################################
##
#O  PosInParent( coll )
##
##  Let <coll> be a collection of elements of a quasigroup Q, and let P
##  be the parent of Q. Then PosInParent( coll )[ i ] is equal to
##  Pos ( Elements( P ), coll[ i ] ).
##  Note that we do not demand that all elements of <coll>
##  belong to the same quasigroup.

InstallMethod( PosInParent, "for collection of quasigroup elements",
    [ IsCollection ],
function( coll )
    if not ForAll( coll, x -> IsQuasigroupElement( x ) ) then
        Error( "LOOPS: <1> must be a list of quasigroup elements." );
    fi;
    return List( coll, x -> x![ 1 ] );
end );

#############################################################################
##
#O  SubquasigroupNC( Q, pos_of_gens )
##
##  This auxiliary function assumes that:
##    a) Q is a quasigroup with Q = Parent( Q )
##    b) pos_of_gens is a nonempty subset of [ 1..Size( Q ) ] (not a sublist)
##    c) pos_of_gens determines a subquasigroup of Q
##  It then returns the corresponding subquasigroup of Q.

InstallMethod( SubquasigroupNC, "for quasigroup and set of integers",
    [ IsQuasigroup, IsSet ],
function( Q, pos_of_gens )
    local subqg, Qtype, elms;
    if IsEmpty( pos_of_gens ) then
        Error( "LOOPS: <2> must be a nonempty subset.");
    fi;
    if not( Q = Parent( Q ) ) then
        Error( "LOOPS: <1> must be its own parent." );
    fi;
    elms := Immutable( Elements( Q ){ pos_of_gens } );
    if IsLoop( Q ) then
        Qtype := NewType( FamilyObj( elms ), IsLoop and IsAttributeStoringRep );
    else
        Qtype := NewType( FamilyObj( elms ), IsQuasigroup and IsAttributeStoringRep );
    fi;
    subqg := Objectify( Qtype, rec( ) );
    SetSize( subqg, Length( pos_of_gens ) );
    SetAsSSortedList( subqg, elms );
    SetParent( subqg, Q );
    if IsLoop( Q ) then
        SetOne( subqg, One( Q ) );
    fi;
    return subqg;
end );

#############################################################################
##
#O  Subquasigroup( Q, gens )
##
##  When <gens> is a nonempty list of elements of a quasigroup <Q>, or the
##  indices of them, it returns the subquasigroup of <Q> generated by <gens>.

InstallMethod( Subquasigroup, "for quasigroup and list of elements",
    [ IsQuasigroup, IsList ],
function( Q, gens )
    local pos_gens, transl, pos_of_elms, relmultgr, subqg;
    # NG: we allow list of indices as well
    # PV: we allow gens to be empty, too. When Q is a loop, the trivial subloop is returned.
    # When Q is a quasigroup, we return []. This is useful when Nuc(Q) is empty in a quasigroup,
    # for instance. 
    if IsEmpty( gens ) then
        if IsLoop( Q ) then
            return SubquasigroupNC( Q, [1] ); # trivial subloop
        fi;
        return [];
    fi;
    if not( ForAll( gens, g -> g in Q ) or ForAll( gens, g-> g in PosInParent( Q ) ) ) then
        Error("LOOPS: <2> must be a list of elements of quasigroup <1> or their indices.");
    fi;
    if not IsInt( gens[1] ) then # convert elements to indices
        pos_gens := PosInParent( gens );
    else
        pos_gens := ShallowCopy( gens );
        gens := Elements( Parent( Q ) ){ gens };
    fi;
    # calculating the subquasigroup
    pos_of_elms := [];
    while pos_gens<>pos_of_elms do
        pos_of_elms := pos_gens;
        transl := Union( LeftSection( Parent( Q ) ){ pos_gens }, RightSection( Parent( Q ) ){ pos_gens } );
        relmultgr := Subgroup( MultiplicationGroup( Parent( Q ) ), transl );
        pos_gens := Set( Orbits( relmultgr, pos_gens )[ 1 ] );
    od;
    subqg := SubquasigroupNC( Parent( Q ), Set( pos_of_elms ) );
    SetGeneratorsOfMagma( subqg, gens );
    return subqg;
end );

#############################################################################
##
#O  Subloop( L, gens )
##
##  When <gens> is a list of elements of a loop <L>, it returns the
##  subloop of <L> generated by <gens>. When <gens> is empty, it returns
##  the trivial subloop of <L>.

InstallMethod( Subloop, "for loop and list of elements",
    [ IsLoop, IsList ],
function( L, gens )
    return Subquasigroup( L, gens );
end );

#############################################################################
##
#O  IsSubquasigroup( Q, S )
##
##  Returns true if <S> is a subquasigorup of a quasigroup <Q>.

InstallMethod( IsSubquasigroup, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( Q, S )
    return Parent( Q ) = Parent( S ) and
           IsSubset( Elements( Q ), Elements( S ) );
end );

#############################################################################
##
#O  IsSubloop( L, S )
##
##  Returns true if <S> is a subloop of a loop <L>.

InstallMethod( IsSubloop, "for two loops",
    [ IsLoop, IsLoop ],
function( L, S )
    return IsSubquasigroup( L, S );
end );

#############################################################################
##
#0  AllSubquasigroups( Q )
##
##  Returns a set of all subquasigroups of a quasigroup <Q>.

InstallMethod( AllSubquasigroups, "for a quasigroup",
    [ IsQuasigroup ],
function( Q )
    # MATH:
    # If S is a subquasigroup of L and x is in L\S, then the cosets S and Sx are disjoint.
    # If S is a proper subquasigroup of L then |L| is at least 2|S|.
    # If S is a proper subquasigroup of L and |S| > |L|/4 then S is maximal in L.
    # If S is a subloop of a power-associative loop L and x  is in L\S then <S,x> = <S,x^m> whenever m is relatively prime to |x|.
    # If S is a subloop of a LIP loop L and x is in L\S then <S,zx> = <S,x> for every z in S.
    # If S is a subloop of a LIP power associative loop L and x is in L\S then <S,z(x^m)> = <S,x> for every z in S and every m relatively prime to |x|.

    local All, Last, New, A, Out, B, x, coprime_powers, n, m;

    # initialization
    All := [];   #all subquasigroups
    if IsLoop( Q ) then
        New := [ Subloop( Q, [ One( Q ) ] ) ]; # the trivial subloop
    else
        New := Set( Elements( Q ), x -> Subquasigroup( Q, [ x ] ) ); # all mono-generated subquasigroups 
    fi;

    # rounds
    repeat
        Append( All, New );
        Last := Filtered( New, A -> Size( A ) <= Size( Q )/4 ); # subquasigroups found in the previous round that are possibly not maximal in Q
        New := [];   # subquasigroups generated in this round
        for A in Last do
            Out := Difference( Elements( Q ), Elements( A ) );
            while not IsEmpty(Out) do
                x := Out[ 1 ];
                Out := Out{[2..Length(Out)]};
                B := Subquasigroup( Q, Union( Elements( A ), [ x ] ) );
                if not B in New and not B in All then
                    Add( New, B );   # new subquasigroup found
                fi;
                # attempting to reduce the number of elements to be checked. This is critical for speed.
                if Size( B ) < 4*Size( A ) then # A is maximal in B, removing everything in B
                    Out := Difference( Out, Elements( B ) );
                elif IsLoop( Q ) then # additional removal methods for loops
                    coprime_powers := [ 1 ];
                    if IsPowerAssociative( Q ) then
                        n := Order( x );
                        coprime_powers := Filtered( [1..n], m -> Gcd(m,n) = 1 );
                    fi;
                    Out := Difference( Out, List( coprime_powers, m -> x^m ) );
                    if HasLeftInverseProperty( Q ) then
                        for m in coprime_powers do
                            Out := Difference( Out, Elements(A)*(x^m) );
                        od;
                    fi;
                    if HasRightInverseProperty( Q ) then
                        for m in coprime_powers do
                            Out := Difference( Out, (x^m)*Elements(A) );
                        od;
                    fi;
                fi; # end of removal
            od; # end of cycle for x
        od; # end of cycle for A
    until IsEmpty( New );

    # finishing
    if not Q in All then Add( All, Q ); fi;
    return All;

end);

#############################################################################
##
#0  AllSubloops( L )
##
##  Returns a set of all subloops of a loop <L>.

InstallMethod( AllSubloops, "for a loop",
    [ IsLoop ],
function( L )
    return AllSubquasigroups( L );
end);

#############################################################################
##
#O  RightCosetsNC( L, S )
##
##  Returns a list of duplicate-free right cosets S*u, for u in L.

# (PROG) RightCosets(L,S) is implemented as a global function in GAP. It
# checks if S is a subset of L and then calls operation RightCosetsNC.
# LeftCosets is not implemented in GAP.
InstallOtherMethod( RightCosetsNC, "for two loops",
    [ IsLoop, IsLoop ],
function( L, S )
    local R, cosets, p, last_coset;
    if not IsSubloop( L, S ) then
        Error( "LOOPS: <2> must be a subloop of <1>" );
    fi;
    R := RightSection( L );
    cosets := [];
    while not IsEmpty( R ) do
        p := R[1];
        last_coset := List( S, x -> x^p );
        R := Filtered( R, r -> not One(L)^r in last_coset );
        Add(cosets, last_coset);
    od;
    return cosets;
end);

#############################################################################
##
#O  RightTransversal( L, S )
##
##  Returns a right transversal for the subloop S of loop L.

InstallMethod( RightTransversal, "for two loops",
    [ IsLoop, IsLoop ],
function( L, S )
    return List( RightCosetsNC( L, S ), x -> x[1] );
end);

#############################################################################
##  NUCLEUS, COMMUTANT, CENTER
##  -------------------------------------------------------------------------

#############################################################################
##
#A  LeftNucleus( Q )
##
##  The left nucleus of quasigroup <Q>.

InstallMethod( LeftNucleus, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local S, LS, n, e;
    LS := LeftSection( Q );
    n := Size( Q );
    e := Elements( Q );
    S := Filtered( [ 1..n ], i -> ForAll( [ 1..n ], j ->
        LS[ j ]*LS[ i ] = LS[ Position( e, e[ i ]*e[ j ] ) ] ) );
    S := Subquasigroup( Q, Elements( Q ){ S } );
    if Size(S)>0 then
        SetIsAssociative( S, true );
    fi;
    return S;
end);

#############################################################################
##
#A  MiddleNucleus( Q )
##
##  The middle nucleus of quasigroup <Q>.

InstallMethod( MiddleNucleus, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local S, LS, n, e;
    LS := LeftSection( Q );
    n := Size( Q );
    e := Elements( Q );
    S := Filtered( [ 1..n ], i -> ForAll( [ 1..n ], j ->
        LS[ i ]*LS[ j ] = LS[ Position( e, e[ j ]*e[ i ] ) ] ) );
    S := Subquasigroup( Q, Elements( Q ){ S } );
    if Size(S)>0 then
        SetIsAssociative( S, true );
    fi;   
    return S;
end);

#############################################################################
##
#A  RightNucleus( Q )
##
##  The right nucleus of quasigroup <Q>.

InstallMethod( RightNucleus, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local S, RS, n, e;
    RS := RightSection( Q );
    n := Size( Q );
    e := Elements( Q );
    S := Filtered( [ 1..n ], i -> ForAll( [ 1..n ], j ->
        RS[ j ]*RS[ i ] = RS[ Position( e, e[ j ]*e[ i ] ) ] ) );
    S := Subquasigroup( Q, Elements( Q ){ S } );
    if Size(S)>0 then
        SetIsAssociative( S, true );
    fi;
    return S;
end);

#############################################################################
##
#A  Nuc( Q )
##
##  The nucleus of quasigroup <Q>.

InstallMethod( Nuc, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local N, S;
    N := Intersection( Elements( LeftNucleus( Q ) ),
        Elements( RightNucleus( Q ) ), Elements( MiddleNucleus( Q ) ) );
    S := Subquasigroup( Q, N );    
    if Size(S)>0 then 
        SetIsAssociative( S, true );
    fi;
    return S;
end);

#############################################################################
##
#A  Commutant( Q )
##
##  The commutant of quasigroup <Q>.

InstallMethod( Commutant, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    return Elements( Q ){ Filtered( [1..Size( Q )], i
        -> LeftSection( Q )[ i ] = RightSection( Q )[ i ] ) };
end);

#############################################################################
##
#A  Center( Q )
##
##  The center of a quasigroup <Q>.

# (PROG) setting rank high to make sure that Center for
# IsMagma and IsCommutative is not called first
InstallOtherMethod( Center, "for quasigroup",
    [ IsQuasigroup ], 1*SUM_FLAGS + 20,
function( Q )
    local S;
    S := Intersection( Nuc( Q ), Commutant( Q ) );
    S := Subquasigroup( Q, S );
    if Size( S ) > 0 then
        SetIsAssociative( S, true );
        SetIsCommutative( S, true );
    fi;
    return S;
end);

#############################################################################
##
#A  AssociatorSubloop( L )
##
##  Returns the associator subloop of the loop <L>, i.e., the subloop of <L>
##  generated by all associators.

InstallOtherMethod( AssociatorSubloop, "for Loop",
    [ IsLoop ],
function( L )
    local LeftInn, gens;
    LeftInn := LeftInnerMappingGroup( L );
    gens := List( L, x-> LeftDivision( x, Orbit( LeftInn, x ) ) );
    return NormalClosure( L, Union( gens ) );
end);

#############################################################################
##  EXPONENT
##  -------------------------------------------------------------------------

#############################################################################
##
#A  Exponent( L )
##
##  Returns the exponent of the loop <L>. Assumes that <L> is power
##  associative.

InstallMethod( Exponent, "for loop",
    [ IsLoop ],
function( L )
    local ords;
    ords := List( Elements( L ), x -> Order( x ) );
    return Lcm( ords );
end );

#############################################################################
##  NORMALITY
##  -------------------------------------------------------------------------

#############################################################################
##  
#F  IsNormal( L, S ) 
##     
##  Returns true if <S> is a normal subloop of loop <L>.

InstallOtherMethod( IsNormal, 
    "for loops",
    [ IsLoop, IsLoop ],
function( L, S )
    local upos;
    if not( IsSubloop( L, S ) ) then 
        Error( "LOOPS: <2> must be a subloop of <1>." );
    fi;
    upos := PosInParent( S );
    return ForAll( GeneratorsOfGroup( InnerMappingGroup( L ) ),
            g -> upos = OnSets( upos, g ) );
end);

#############################################################################
##  
#F  NormalClosure( L, S ) 
##     
##  When <S> is a subset of a loop <L> or a subloop of < L >, returns the 
##  normal closure of <S> in <L>, i.e., the smallest normal subloop of <L> 
##  containing <S>.

InstallOtherMethod( NormalClosure,
    "for loops",
    [ IsLoop, IsLoop ],
function( L, S )
    if not IsSubloop( L, S ) then
        Error( "LOOPS: <2> must be a subloop of <1>." );
    fi;
    return NormalClosure( L, Elements( S ) );
end);

InstallOtherMethod( NormalClosure, 
    "for loops",
    [ IsLoop, IsCollection ],
function( L, gens )
    local transl_of_gens, nc_mltgr;
    if ForAny( gens, x -> not x in L ) then
        Error( "LOOPS: <2> must be a subset of <1>." );
    fi;
    transl_of_gens := List( gens, x -> LeftTranslation( L, x ) );
    nc_mltgr := Group( Union( 
        GeneratorsOfGroup( InnerMappingGroup( L ) ), transl_of_gens 
    ) );
    return SubquasigroupNC( Parent( L ), Set( Orbit( nc_mltgr, 1 ) ) );
end);

#############################################################################
##  
#P  IsSimple( L ) 
##     
##  Returns true if <L> is a simple loop.

InstallOtherMethod( IsSimple, 
    "for a loop",
    [ IsLoop ],
function( L )
    return IsPrimitive( MultiplicationGroup( L ) );
end);

#############################################################################
##  FACTOR LOOP
##  -------------------------------------------------------------------------

#############################################################################
##  
#O  FactorLoop( L, N ) 
##     
##  When <N> is a normal subloop of loop <L>, returns the factor loop L/N.

InstallMethod( FactorLoop, "for two loops",
    [ IsLoop, IsLoop ],
function( L, N )
    local cosets, ltrans, f_ct, f_loop;
    if not( IsNormal( L, N ) ) then 
        Error( "LOOPS: <2> must be normal in <1>." );
    fi;
    cosets := Set( Orbit( MultiplicationGroup( L ), Elements( N ), OnSets ) );
    f_ct := List( cosets, x -> List( cosets, y ->
        First( [1..Length(cosets)], i->x[1]*y[1] in cosets[i] )
    ) );
    f_loop := LoopByCayleyTable( f_ct );
    return f_loop;
end);

InstallOtherMethod( \/, "for two loops",
    IsIdenticalObj,
    [ IsLoop, IsLoop ], 0,
function( L, N )
    return FactorLoop( L, N );
end );

#############################################################################
##  
#O  NaturalHomomorphismByNormalSubloop( L, N ) 
##     
##  When <N> is a normal subloop of loop <L>, returns the natural projection
##  from <L> onto the factor loop L/N.

InstallMethod( NaturalHomomorphismByNormalSubloop, "for two loops",
    [ IsLoop, IsLoop ],
function( L, N )
    local cosets, in_coset, f_ct, f_loop, map;
    if not( IsNormal( L, N ) ) then
      Error( "LOOPS: <2> must be normal in <1>." );
    fi;
    cosets := Set( Orbit( MultiplicationGroup( L ), Elements( N ), OnSets ) );
    in_coset := function( n )
        return First( [1..Size( L )/Size( N )], i -> n in cosets[ i ] );
    end;
    f_ct := List( cosets, x -> List( cosets, y -> in_coset( x[1]*y[1] ) ) );
    f_loop := LoopByCayleyTable( f_ct );
    map := function( x )
        return Elements( f_loop )[ in_coset( x ) ];
    end;
    return MappingByFunction( L, f_loop, map );
end );

###########################################################################
##  NILPOTENCY
##  -------------------------------------------------------------------------

#############################################################################
##  
#A  NilpotencyClassOfLoop( L ) 
##     
##  Returns the nilpotency class of loop <L>. When <L> is not nilpotent,
##  returns fail.

InstallMethod( NilpotencyClassOfLoop, "for a loop",
    [ IsLoop ],
function( L )
    local n;
    if Size( L ) = 1 then 
        return 0; 
    elif 
      Size( Centre( L ) ) = 1 then 
        return fail; 
    else 
        n := NilpotencyClassOfLoop( L / Centre( L ) );
        if n = fail then 
            return fail;
        else
            return n + 1;
        fi;
    fi;
end );

#############################################################################
##  
#P  IsNilpotent( L ) 
##     
##  Returns true if <L> is nilpotent.

InstallOtherMethod( IsNilpotent, "for a loop", 
    [ IsLoop ], 
function( L ) 
    return NilpotencyClassOfLoop( L ) <> fail;
end );

#############################################################################
##  
#P  IsStronglyNilpotent( L ) 
##     
##  Returns true if <L> is strongly nilpotent, i.e., when the multiplication
##  group of <L> is nilpotent.

InstallMethod( IsStronglyNilpotent, "for a loop", 
    [ IsLoop ],
function( L )
    return IsNilpotent( MultiplicationGroup( L ) );
end );

#############################################################################   
##
#A  UpperCentralSeries( L )
##
##  Returns the upper central series of the loop L. 

InstallOtherMethod( UpperCentralSeries, "for a loop",  
        [ IsLoop ], 
        function(L)
    local fmap, Lbar;
    
    if Size(Center(L))=1 then return [ Subloop(L, []) ]; fi;
    fmap := NaturalHomomorphismByNormalSubloop( L, Center( L ) );
    Lbar := Range( fmap );
    return Concatenation(
        List( UpperCentralSeries( Lbar ), s -> Subloop(L, PreImage( fmap, s ) ) ),
        [ Subloop( L, [] ) ]
    );
end);

#############################################################################   
##
#A  LowerCentralSeries( L )
##
##  Returns the lower central series of the loop L. 

InstallOtherMethod( LowerCentralSeries, "for a loop",  
        [ IsLoop ], 
        function(L)
    local last, new, ret, x;
    
    last := [];
    new := L;
    ret := [];
    while new <> last do
        last := ShallowCopy(new);
        Add( ret, last );
        new := [ One(L) ];
        for x in last do
            new := Union( new, Orbit(InnerMappingGroup(L),x)/x );
        od;
        new := NormalClosure(L, new);
    od;
    return ret;
end);

#############################################################################
##  SOLVABILITY
##  -------------------------------------------------------------------------

#############################################################################
##  
#P  IsSolvable( L ) 
##     
##  Returns true if <L> is solvable.

InstallOtherMethod( IsSolvable, "for a loop",
        [ IsLoop ],
function( L )
    local N;
    N := DerivedSubloop( L );
    if Size( N ) = 1 then return true; fi;
    if N = L then return false; fi;
    return IsSolvable( N ) and IsSolvable( L / N );
end );

#############################################################################
##  
#F  DerivedSubloop( L ) 
##     
##  Returns the derived subloop of <L>.

InstallMethod( DerivedSubloop, "for a loop", 
    [ IsLoop ],
function( L )
    local D;
    D := Orbit( DerivedSubgroup( MultiplicationGroup( L ) ), One( L ) );
    return Subloop( L, D );
end );

#############################################################################
##  
#A  DerivedLength( L ) 
##     
##  Returns the derived length of <L>.

InstallOtherMethod( DerivedLength, "for a loop", 
    [ IsLoop ], 
function( L ) 
    local D;
    D := DerivedSubloop( L );
    if Size( D ) = Size ( L ) then
        return 0;
    else
        return DerivedLength( D ) + 1;
    fi;
end );

#############################################################################
##  
#A  FrattiniSubloop( L ) 
##     
##  Returns the Frattini subloop of a strongly nilpotent loop <L>.

InstallMethod( FrattiniSubloop, "for a strongly nilpotent loop", 
    [ IsLoop ],
function( L )
    local F;
    if IsStronglyNilpotent( L ) then
        F := Orbit( FrattiniSubgroup( MultiplicationGroup( L ) ), One( L ) );
        return Subloop( L, F );
    else
        TryNextMethod();
        return;
    fi;
end );

#############################################################################
##  
#A  FrattinifactorSize( L ) 
##     
##  Returns the Frattini factor size of loop <L>, i.e., the index of
##  the Frattini subloop of <L> in <L>.

InstallOtherMethod( FrattinifactorSize, "for a strongly nilpotent loop", 
    [ IsLoop ],
function( L )
    if IsStronglyNilpotent( L ) then
        return Size( L ) / Size( FrattiniSubloop( L ) );
    else
        TryNextMethod();
        return;
    fi;
end );
