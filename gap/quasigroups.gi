#############################################################################
##
#W  quasigroups.gi  Representing, creating and displaying quasigroups [loops]
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  TESTING MULTIPLICATION TABLES
##  -------------------------------------------------------------------------

#############################################################################
##
#O  IsQuasigroupTable( ls )
##
##  Returns true if <ls> is an n by n latin square with n distinct
##  integral entries.

InstallMethod( IsQuasigroupTable, "for matrix",
    [ IsMatrix ],
function( ls )
    local first_row;
    # checking rows
    first_row := Set( ls[ 1 ] );
    if not Length( first_row ) = Length( ls[ 1 ] ) then return false; fi;
    if ForAll( ls, row -> Set( row ) = first_row ) = false then return false; fi;
    # checking columns
    ls := TransposedMat( ls );
    first_row := Set( ls[ 1 ] );
    if not Length( first_row ) = Length( ls[ 1 ] ) then return false; fi;
    if ForAll( ls, row -> Set( row ) = first_row ) = false then return false; fi;
    return true;
end );

#############################################################################
##
#O  IsLoopTable( ls )
##
##  Returns true if <ls> is a normalized latin square. An n by n latin square
##  is normalized if the first row and first column read the same, and the
##  entries in the first row are ordered.

InstallMethod( IsLoopTable, "for matrix",
    [ IsMatrix ],
function( ls )
    if not IsQuasigroupTable( ls ) then return false; fi;
    return Set( ls[ 1 ] ) = ls[ 1 ]
        and ls[ 1 ] = List( [1..Length(ls)], i -> ls[ i ][ 1 ] );
end );

#############################################################################
##
#O CanonicalCayleyTable( ls )
##
## Returns a Cayley table isomorphic to <ls>, in which the entries of ls
## have been replaced by numerical values 1, ..., n in the following way:
## Let e_1 < ... < e_n be all distinct entries of ls. Then e_i is renamed
## to i. In particular, when {e_1,...e_n} = {1,...,n}, the operation
## does nothing.

InstallMethod( CanonicalCayleyTable, "for matrix",
    [ IsMatrix ],
function( ls )
    local n, entries, i, j, T;
    n := Length( ls );
    # finding all distinct entries in the table
    entries := [];
    for i in [1..n] do for j in [1..n] do
        AddSet( entries, ls[ i ][ j ] );
    od; od;
    # renaming the entries and making a mutable copy, too
    T := List( [1..n], i -> [1..n] );
    for i in [1..n] do for j in [1..n] do
       T[ i ][ j ] := Position( entries, ls[ i ][ j ] );
    od; od;
    return T;
end );

#############################################################################
##
#O  NormalizedQuasigroupTable( ls )
##
##  Given a latin square <ls>, returns the corresponding normalized
##  latin square with entries 1, ..., n.

InstallMethod( NormalizedQuasigroupTable, "for matrix",
    [ IsMatrix ],
function( ls )
    local T, perm;
    if not IsQuasigroupTable( ls ) then
        Error( "LOOPS: <1> must be a latin square." );
    fi;
    # renaming the entries to be 1, ..., n
    T := CanonicalCayleyTable( ls );
    # permuting the columns so that the first row reads 1, ..., n
    perm := PermList( T[ 1 ] );
    T := List( T, row -> Permuted( row, perm ) );
    # permuting the rows so that the first column reads 1, ..., n
    return Set( T );
end );

#############################################################################
##  CREATING QUASIGROUPS AND LOOPS MANUALLY
##  -------------------------------------------------------------------------

#############################################################################
##
#A  CayleyTable( Q )
##
##  Returns the Cayley table of the quasigroup <Q>

InstallMethod( CayleyTable, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local elms, parent_elms;
    elms := Elements( Q );
    parent_elms := Elements( Parent( Q ) );
    return List( elms, x-> List( elms, y -> Position( parent_elms, x * y ) ) );
end );

#############################################################################
##
#O  QuasigroupByCayleyTable( ct )
##
##  Returns quasigroup with multiplication table <ct>.

InstallMethod( QuasigroupByCayleyTable, "for matrix",
    [ IsMatrix ],
function( ct )

    local F, Q, elms, n;
    if not IsQuasigroupTable( ct ) then
        Error( "LOOPS: <1> must be a latin square." );
    fi;
    # Making sure that entries are 1, ..., n
    ct := CanonicalCayleyTable( ct );
    # constructing the family
    F := NewFamily( "QuasigroupByCayleyTableFam", IsQuasigroupElement );
    # installing data for the family
    n := Length ( ct );
    F!.size := n;
    elms := Immutable( List( [1..n], i -> Objectify(
        NewType( F, IsQuasigroupElement and IsQuasigroupElmRep), [ i ] ) ) );
    F!.set := elms;
    F!.cayleyTable := ct;
    F!.names := "q";
    # creating the quasigroup
    Q := Objectify( NewType( FamilyObj( elms ),
        IsQuasigroup and IsAttributeStoringRep ), rec() );
    # setting attributes for the quasigroup
    SetSize( Q, n );
    SetAsSSortedList( Q, elms );
    SetParent( Q, Q );
    SetCayleyTable( Q, ct );
    return Q;
end );

#############################################################################
##
#O  LoopByCayleyTable( ct )
##
##  Returns loop with multiplication table <ct>.

InstallMethod( LoopByCayleyTable, "for matrix",
    [ IsMatrix ],
function( ct )
    local F, L, elms, n;
    if not IsLoopTable( ct ) then
        Error( "LOOPS: <1> must be a normalized latin square." );
    fi;
    # Making sure that the entries are 1, ..., n.
    # The table will remain normalized.
    ct := CanonicalCayleyTable( ct );
    # constructing the family
    F := NewFamily( "LoopByCayleyTableFam", IsLoopElement );
    # installing the data for the family
    n := Length ( ct );
    F!.size := n;
    elms := Immutable( List( [1..n], i -> Objectify(
        NewType( F, IsLoopElement and IsLoopElmRep), [ i ] ) ) );
    F!.set := elms;
    F!.cayleyTable := ct;
    F!.names := "l";
    # creating the loop
    L := Objectify( NewType( FamilyObj( elms ),
        IsLoop and IsAttributeStoringRep ), rec() );
    # setting attributes for the loop
    SetSize( L, n );
    SetAsSSortedList( L, elms );
    SetParent( L, L );
    SetCayleyTable( L, ct );
    SetOne( L, elms[ 1 ] );
    return L;
end );

#############################################################################
##
#O  SetQuasigroupElmName( Q, name )
##
##  Changes the name of elements of quasigroup or loop <Q> to <name>

InstallMethod( SetQuasigroupElmName, "for quasigroup and string",
    [ IsQuasigroup, IsString ],
function( Q, name )
    local F;
    F := FamilyObj( Elements( Q )[ 1 ] );
    F!.names := name;
end);

#############################################################################
##
#O  CanonicalCopy( Q )
##
##  Returns a canonical copy of <Q>, that is, an isomorphic object <O> with
##  canonical multiplication table and Parent( <O> ) = <O>.
##  (PROG) Properties and attributes are lost!

InstallMethod( CanonicalCopy, "for quasigroup or loop",
    [ IsQuasigroup ],
function( Q )
    if IsLoop( Q ) then
        return LoopByCayleyTable( CayleyTable( Q ) );
    fi;
    return QuasigroupByCayleyTable( CayleyTable( Q ) );
end);


#############################################################################
##  CREATING QUASIGROUPS AND LOOPS FROM A FILE
##  -------------------------------------------------------------------------

#############################################################################
##
#F  LOOPS_ReadCayleyTableFromFile( filename, replace_by_spaces )
##
##  Auxiliary function. Reads the content of <filename> and tries to
##  interpret the data as a multiplication table, according to the rules
##  summarized below. If successful, it returns the multiplication table as
##  an n by n array.
##  ALGORITHM:
##  1) the content of the file is read into one string
##  2) all end-of-lines and all characters in the string <replace_by_spaces>
##     are replaced by spaces
##  3) the string is split into chunks, where chunk = maximal substring
##     without spaces
##  4) the number n of distinct chunks is found. If the number of chunks
##     is not n^2, error is announced and function terminates
##  5) a numerical value 1 .. n is assigned to each chunk, depending on
##     its position among the distinct chunks
##  6) multiplication table is constructed and returned

InstallGlobalFunction( LOOPS_ReadCayleyTableFromFile,
function( filename, replace_by_spaces)

    local input, s, i, chunks, started, starting_pos, z, j, distinct_chunks, c, n, T;

    if not ( IsString( filename) and IsString( replace_by_spaces ) ) then
        Error( "LOOPS: <1> must be a file name, and <2> must be a string." );
    fi;

    input := InputTextFile( filename );
    if input = fail then Error( "LOOPS: <1> is not a valid file name." ); fi;

    s := ReadAll( input );
    
    CloseStream(input);
    
    # removing end-of-lines, etc.
    for i in [1..Length( s )] do
        if (s[ i ] = '\n') or (s[ i ] in replace_by_spaces) then
             s[ i ] := ' ';
        fi;
    od;
    s[ Length( s ) + 1 ] := ' '; #to make sure that string ends with space

    #parsing string into chunks separated by spaces
    chunks := [];
    started := false; starting_pos := 0;
    for i in [1..Length( s )] do
        if not started then
            if not s[ i ] = ' ' then
                started := true;
                starting_pos := i;
            fi;
        else
            if s[ i ] = ' ' then #end of chunk
                z := "";
                for j in [ starting_pos..i-1 ] do
                    z[ j - starting_pos + 1 ] := s[ j ];
                od;
                Add( chunks, z );
                started := false;
            fi;
        fi;
    od;

    distinct_chunks := [];
    for c in chunks do if not c in distinct_chunks then
        Add( distinct_chunks, c );
    fi; od;

    n := Length( distinct_chunks );
    if not Length( chunks ) = n^2 then
        Error( "LOOPS: The data in the file cannot be arranged into a square table." );
    fi;

    T := List( [1..n], i -> 0*[1..n] );
    for i in [1..n] do for j in [1..n] do
        T[ i ][ j ] := Position( distinct_chunks, chunks[ (i-1)*n + j ] );
    od; od;

    return T;

end);

#############################################################################
##
#O  QuasigroupFromFile( filename, replace] )
##
##  Calls LOOPS_ReadCayleyTableFromFile( filename, replace ) in order to return
##  the quasigroup with multiplication table in file <filename>.

InstallMethod( QuasigroupFromFile, "for string and string",
    [ IsString, IsString ],
function( filename, replace )
    return QuasigroupByCayleyTable( LOOPS_ReadCayleyTableFromFile( filename, replace ) );
end );


#############################################################################
##
#O  LoopFromFile( filename , replace] )
##
##  Calls LOOPS_ReadCayleyTableFromFile( filename, replace ) in order to return
##  the loop with multiplication table in file <filename>.

InstallMethod( LoopFromFile, "for string and string",
    [ IsString, IsString ],
function( filename, replace )
    return LoopByCayleyTable(  LOOPS_ReadCayleyTableFromFile( filename, replace ) );
end );

#############################################################################
##  CREATING QUASIGROUPS AND LOOPS BY SECTIONS AND FOLDERS
##  -------------------------------------------------------------------------

#############################################################################
##
#O  CayleyTableByPerms( perms )
##
##  Given a set <perms> of n permutations of an n-element set X, returns
##  n by n Cayley table ct such that ct[i][j] = X[j]^perms[i].
##  The operation is safe only if at most one permutation of <perms> is
##  the identity permutation, and all other permutations of <perms>
##  move all points of X.

InstallMethod( CayleyTableByPerms,
    "for a list of permutations",
    [ IsPermCollection ],
function( perms )
    local n, pts, max;
    n := Length( perms );
    if n=1 then
        return [ [ 1 ] ];
    fi;
    # one of perms[ 1 ], perms[ 2 ] must move all points
    pts := MovedPoints( perms[ 2 ] );
    if pts = [] then
        pts := MovedPoints( perms[ 1 ] );
    fi;
    max := Maximum( pts );
    # we permute the whole interval [1..max] and then keep only those coordinates corresponding to pts
    return List( perms, p -> Permuted( [1..max], p^(-1) ){ pts } );
end);

#############################################################################
##
#O  QuasigroupByLeftSection( sect )
##
##  Returns the quasigroup whose left section is the list of permutations
##  <sect>.

InstallMethod( QuasigroupByLeftSection,
    "for a set of left translation maps",
    [ IsPermCollection ],
function( sect )
    return QuasigroupByCayleyTable( CayleyTableByPerms( sect ) );
end);

#############################################################################
##
#O  LoopByLeftSection( sect )
##
##  Returns the loop whose left section is the list of permutations <sect>.
##  Since the order of translations in <sect> is determined by their
##  image of the neutral element 1, we disregard the order.

InstallMethod( LoopByLeftSection,
    "for a set of left translation maps",
    [ IsPermCollection ],
function( sect )
    return LoopByCayleyTable( Set ( CayleyTableByPerms( sect ) ) );
end);

#############################################################################
##
#O  QuasigroupByRightSection( sect )
##
##  Returns the quasigroup whose right section is the list of permutations
##  <sect>.

InstallMethod( QuasigroupByRightSection,
    "for a set of left translation maps",
    [ IsPermCollection ],
function( sect )
    return QuasigroupByCayleyTable( TransposedMat ( CayleyTableByPerms( sect ) ) );
end);

#############################################################################
##
#O  LoopByRightSection( sect )
##
##  Returns the loop whose right section is the list of permutations <sect>.
##  Since the order of translations in <sect> is determined by their
##  image of the neutral element 1, we disregard the order.

InstallMethod( LoopByRightSection,
    "for a set of left translation maps",
    [ IsPermCollection ],
function( sect )
    return LoopByCayleyTable( TransposedMat ( Set ( CayleyTableByPerms( sect ) ) ) );
end);

#############################################################################
##
#O  LOOPS_CayleyTableByRightFolder( G, H, T )
##
##  Auxiliary operation.
##
##  A right folder is a triple (G,H,T) such that G is a group, H is 
##  a subgroup of G, and T is a right transversal to H in G.
##
##  Returns the multiplication table on {Hx: x in G} by Ht*Hs = H(ts).
##
##  The multiplication table is a quasigroup if and only if
##  T is a right transversal to every conjugate H^g in G.

InstallGlobalFunction( LOOPS_CayleyTableByRightFolder, 
function( G, H, T )
    local act, nT, actT, i, p, ct;
    # act = action of G on right cosets G/H
    act := ActionHomomorphism( G, RightCosets( G, H ), OnRight );
    nT := Length( T );
    # actT = permutations on G/H induced by elements of T
    actT := [1..nT];
    for i in [1..nT] do
        actT[ i ] := T[ i ]^act;
    od;
    # the order of right cosets determined by T might not agree with the default order of right cosets ...
    p := PermList( List( [1..nT], i -> 1^actT[ i ] ) );
    ct := List( [1..nT], i -> ListPerm( p * actT[ i ] * p^(-1) ) );
    for i in [1..nT] do
        if ct[ i ] = [] then # this can happen since ListPerm( () ) = []
            ct[ i ] := [1..nT];
        fi;
    od;
    return TransposedMat( ct );
end);

#############################################################################
##
#O  QuasigroupByRightFolder( G, H, T )
##
##  See CayleyTableByRightFolder. We do not check if the right folder
##  is a quasigroup right folder.

InstallMethod( QuasigroupByRightFolder,
    "for a group, a subgroup and right transversal",
    [ IsGroup, IsGroup, IsMultiplicativeElementCollection ],
function( G, H, T )
    return QuasigroupByCayleyTable( LOOPS_CayleyTableByRightFolder( G, H, T ) );
end);

#############################################################################
##
#O  LoopByRightFolder( G, H, T )
##
##  See CayleyTableByRigthFolder. We do not check if the right folder
##  is a loop right folder.

InstallOtherMethod( LoopByRightFolder,
    "for a group, a subgroup and right transversal",
    [ IsGroup, IsGroup, IsMultiplicativeElementCollection ],
function( G, H, T )
    return LoopByCayleyTable( LOOPS_CayleyTableByRightFolder( G, H, T ) );
end);

#############################################################################
##  CONVERSIONS
##  -------------------------------------------------------------------------

#############################################################################
##
#O  IntoQuasigroup( M )
##
##  Given a magma, returns the corresponding quasigroup, if possible.

InstallMethod( IntoQuasigroup, "for magma",
    [ IsMagma ],
function( M )
    local ct;
    if IsQuasigroup( M ) then # leave quasigroups and loops intact
        return M;
    fi;
    # magma, not necessarily a quasigroup
    ct := MultiplicationTable( Elements( M ) );
    if IsQuasigroupTable( ct ) then 
        return QuasigroupByCayleyTable( ct );
    fi;
    return fail;
end);

#############################################################################
##
#O  PrincipalLoopIsotope( Q, f, g )
##
##  Let Q be a quasigroup and f, g elements of Q.
##  Define new operation on Q by x+y = R^{-1}(g)(x) * L^{-1}(f)(y).
##  Then (Q,+) is a loop with neutral element f*g.
##  We return isomorphic copy of (Q,+) via the isomorphism (1,f*g).

InstallMethod( PrincipalLoopIsotope,
    "for quasigroup and two quasigroup elements",
    [ IsQuasigroup, IsQuasigroupElement, IsQuasigroupElement ],
function( Q, f, g )
    local n, L, R, i, j, ct, p;
    if not (f in Q and g in Q) then
        Error("LOOPS: <2> and <3> must be elements of quasigroup <1>.");
    fi;
    # constructing new multiplication
    n := Size( Q );
    ct := List( [1..n], i -> [1..n] );
    L := Inverse( LeftTranslation( Q, f ) );    # inverse of left translation by f
    R := Inverse( RightTranslation( Q, g ) );   # inverse or right translation by g
    for i in [1..n] do for j in [1..n] do
        ct[ i ][ j ] := CayleyTable( Q )[ i^R ][ j^L ];
    od; od;
    # the neutral element of ct is now f*g. We apply isomorphism (1, f*g).
    p := Position(Q, f*g);
    if p>1 then
        p := (1, p); # note that p is its own inverse
        ct := List([1..n], i-> List([1..n], j -> ( ct[ i^p ][ j^p ] )^p ) );
    fi;
    return LoopByCayleyTable( ct );
end);

#############################################################################
##
#O  IntoLoop( M )
##
##  Given a magma, returns the corresponding loop, if possible.

InstallMethod( IntoLoop, "for magma",
    [ IsMagma ],
function( M )
    local e, p, ct;
    if IsLoop( M ) then # loops are left intact
        return M;
    fi; 
    # magma, not necessarily a loop
    M := IntoQuasigroup( M );
    if M = fail then
        return fail;
    fi;
    # quasigroup, not necessarily a loop
    e := MultiplicativeNeutralElement( M );
    if e = fail then # no neutral element, use principal isotope
        return PrincipalLoopIsotope( M, Elements( M )[ 1 ], Elements( M )[ 1 ] );
    fi;
    # quasigroup with neutral element, i.e., a loop
    p := Position( M, e );
    if p>1 then
        p := (1,p); # note that p is its own inverse
        ct := List([1..Size(M)], i-> List([1..Size(M)], j ->
            ( CayleyTable( M )[ i^p ][ j^p ] )^p
        ) );
    else
        ct := CayleyTable( M );
    fi;
    return LoopByCayleyTable( ct );
end );

#############################################################################
##
#O  IntoGroup( M )
##
##  Given a magma <M>, returns the corresponding group, if possible.

InstallOtherMethod( IntoGroup, "for magma",
    [ IsMagma ],
function( M )
    if IsGroup( M ) then # groups are left intact
        return M;
    fi;
    # magma, not necessarily a group
    M := IntoLoop( M );
    if M=fail or (not IsAssociative( M ) ) then
        return fail;
    fi;
    # group
    return RightMultiplicationGroup( M );
end); 

#############################################################################
##  PRODUCTS OF QUASIGROUPS AND LOOPS
##  -------------------------------------------------------------------------

#############################################################################
##
#F  DirectProduct( Q1, Q2, ..., Qn )
##
##  Returns the direct product of quasigroups <Q1>, <Q2>, ... , <Qn>.
##  The quasigroups can be declared as quasigroups, loops or groups.

# The following is necessary due to implementation of DirectProduct for
# groups in GAP. The idea is as follows:
# We want to calculate direct product of quasigroups, loops and groups.
# If only groups are on the list, standard GAP DirectProduct will take care
# of it. If there are also some quasigroups or loops on the list,
# we must take care of it.
# However, we do not know if such a list will be processed with
# DirectProductOp( <IsList>, <IsGroup> ), or
# DirectProductOp( <IsList>, <IsQuasigroup> ),
# since this depends on which algebra is listed first.
# We therefore take care of both situations.

InstallOtherMethod( DirectProductOp, "for DirectProduct( <IsList>, <IsGroup> )",
    [ IsList, IsGroup],
function( list, first )
    local L, p;

    # Check the arguments.
    if IsEmpty( list ) then Error( "LOOPS: <1> must be nonempty." ); fi;
    if not ForAny( list, IsQuasigroup ) then
        # there are no quasigroups or loops on the list
        TryNextMethod();
    fi;
    if ForAny( list, G -> (not IsGroup( G )) and (not IsQuasigroup( G ) ) ) then
        # there are other objects beside groups, loops and quasigroups on the list
        TryNextMethod();
    fi;

    # all arguments are groups, quasigroups or loops, and there is at least one loop
    # making sure that a loop is listed first so that this method is not called again
    for L in list do
        if not IsGroup( L ) then
            p := Position( list, L );
            list[ 1 ] := L;
            list[ p ] := first;
            break;
        fi;
    od;

    return DirectProductOp( list, list[ 1 ] );
end);

InstallOtherMethod( DirectProductOp, "for DirectProduct( <IsList>, <IsQuasigroup> )",
    [ IsList, IsQuasigroup ],
function( list, dummy )

    local group_list, quasigroup_list, group_product, are_all_loops,
        n, i, nL, nM, TL, TM, T, j, k, s;

    # check the arguments
    if IsEmpty( list ) then
        Error( "LOOPS: <1> must be nonempty." );
    elif ForAny( list, G -> (not IsGroup( G )) and (not IsQuasigroup( G ) ) ) then
        TryNextMethod();
    fi;

    # only groups, quasigroups and loops are on the list, with at least one non-group
    group_list := Filtered( list, G -> IsGroup( G ) );
    quasigroup_list := Filtered( list, G -> IsQuasigroup( G ) );
    if not IsEmpty( group_list ) then   # some groups are on the list
        group_product := DirectProductOp( group_list, group_list[ 1 ] );
        Add( quasigroup_list, IntoLoop( group_product ) );
    fi;
    # keeping track of whether all algebras are in fact loops
    are_all_loops := ForAll( quasigroup_list, IsLoop );

    # now only quasigroups and loops are on the list
    n := Length( quasigroup_list );
    if n=1 then
        return quasigroup_list[ 1 ];
    fi;
    # at least 2 quasigroups and loops; we will not use recursion
    # making all Cayley tables canonical
    for s in [1..n] do
        quasigroup_list[ s ] := QuasigroupByCayleyTable( CanonicalCayleyTable( CayleyTable( quasigroup_list[ s ] ) ) );
    od;
    for s in [2..n] do
        nL := Size( quasigroup_list[ 1 ] );
        nM := Size( quasigroup_list[ s ] );
        TL := CayleyTable( quasigroup_list[ 1 ] );
        TM := CayleyTable( quasigroup_list[ s ] );
        T := List( [1..nL*nM], j->[] );

        # not efficient, but it does the job
        for i in [1..nM] do for j in [1..nM] do for k in [1..nL] do
            Append( T[ (i-1)*nL + k ], TL[ k ] + nL*(TM[i][j]-1) );
        od; od; od;
        quasigroup_list[ 1 ] := QuasigroupByCayleyTable( T );
    od;
    if are_all_loops then
        return IntoLoop( quasigroup_list[1] );
    fi;
    return quasigroup_list[ 1 ];
 end );

#############################################################################
## OPPOSITE QUASIGROUPS AND LOOPS
## --------------------------------------------------------------------------

#############################################################################
##
#O  OppositeQuasigroup( Q )
##
##  Returns the quasigroup opposite to the quasigroup <Q>. 

InstallMethod( OppositeQuasigroup, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    return QuasigroupByCayleyTable( TransposedMat( CayleyTable( Q ) ) );
end );

#############################################################################
##
#O  OppositeLoop( Q )
##
##  Returns the loop opposite to the loop <Q>. 

InstallMethod( OppositeLoop, "for loop",
    [ IsLoop ],
function( Q )
    return LoopByCayleyTable( TransposedMat( CayleyTable( Q ) ) );
end );

#############################################################################
##
#A  Opposite( Q )
##
##  Returns the quasigroup opposite to the quasigroup <Q>. When
##  <Q> is a loop, a loop is returned.

InstallMethod( Opposite, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    if IsLoop( Q ) then
        return LoopByCayleyTable( TransposedMat( CayleyTable( Q ) ) );
    fi;
    return QuasigroupByCayleyTable( TransposedMat( CayleyTable( Q ) ) );
end );

#############################################################################
##  DISPLAYING QUASIGROUPS AND LOOPS
##  -------------------------------------------------------------------------

InstallMethod( ViewObj, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    Print( "<quasigroup of order ", Size( Q ), ">" );
end );

## dangerous for large quasigroups
InstallMethod( PrintObj, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    if HasCayleyTable( Q ) then
        Print( "<quasigroup with multiplication table\n" );
        PrintArray( CayleyTable( Q ) );
    else
        Print( "<quasigroup with elements\n" );
        Print( Elements( Q ) );
    fi;
    Print( ">\n" );
end );

InstallMethod( ViewObj, "for loop",
    [ IsLoop ],
function( L )
    local PrintMe;
    
    PrintMe := function( name, L )
        Print( "<", name, " loop of order ", Size( L ), ">");
    
    end;
    if HasIsAssociative( L ) and IsAssociative( L ) then PrintMe( "associative", L ); 
    elif HasIsExtraLoop( L ) and IsExtraLoop( L ) then PrintMe( "extra", L ); 
    elif HasIsMoufangLoop( L ) and IsMoufangLoop( L ) then PrintMe( "Moufang", L ); 
    elif HasIsCLoop( L ) and IsCLoop( L ) then PrintMe( "C", L ); 
    elif HasIsLeftBruckLoop( L ) and IsLeftBruckLoop( L ) then PrintMe( "left Bruck", L );
    elif HasIsRightBruckLoop( L ) and IsRightBruckLoop( L ) then PrintMe( "right Bruck", L );
    elif HasIsLeftBolLoop( L ) and IsLeftBolLoop( L ) then PrintMe( "left Bol", L );
    elif HasIsRightBolLoop( L ) and IsRightBolLoop( L ) then PrintMe( "right Bol", L );
    elif HasIsAutomorphicLoop( L ) and IsAutomorphicLoop( L ) then PrintMe( "automorphic", L );
    elif HasIsLeftAutomorphicLoop( L ) and IsLeftAutomorphicLoop( L ) then PrintMe( "left automorphic", L );
    elif HasIsRightAutomorphicLoop( L ) and IsRightAutomorphicLoop( L ) then PrintMe( "right automorphic", L );
    elif HasIsLCLoop( L ) and IsLCLoop( L ) then PrintMe( "LC", L );
    elif HasIsRCLoop( L ) and IsRCLoop( L ) then PrintMe( "RC", L );
    elif HasIsLeftAlternative( L ) and IsLeftAlternative( L ) then
        if HasIsRightAlternative( L ) and IsRightAlternative( L ) then
            PrintMe("alternative", L );
        else 
            PrintMe("left alternative", L );
        fi;
    elif HasIsRightAlternative( L ) and IsRightAlternative( L ) then PrintMe( "right alternative", L );
    elif HasIsCommutative( L ) and IsCommutative( L ) then PrintMe( "commutative", L );
    elif HasIsFlexible( L ) and IsFlexible( L ) then PrintMe( "flexible", L);
    else
        # MORE ??
        Print( "<loop of order ", Size( L ), ">" );
    fi;
end );

## dangerous for large loops
InstallMethod( PrintObj, "for loop",
    [ IsLoop ],
function( L )
    if HasCayleyTable( L ) then
        Print( "<loop with multiplication table\n" );
        PrintArray( CayleyTable( L ) );
    else
        Print( "<loop with elements\n" );
        Print( Elements( L ) );
    fi;
    Print( ">\n" );
end );
