#############################################################################
##
#W  quasigrp.gi      Basic methods for q & l     G. P. Nagy / P. Vojtechovsky
##  
#H  @(#)$Id: quasigrp.gi, v 1.0.0 2005/08/28 gap Exp $
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
    if not Length( first_row ) = Length( ls[ 1 ] ) then return fail; fi;
    if ForAll( ls, row -> Set( row ) = first_row ) = false then return false; fi;
    # checking columns
    ls := TransposedMat( ls );
    first_row := Set( ls[ 1 ] );
    if not Length( first_row ) = Length( ls[ 1 ] ) then return fail; fi;
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
## to i.

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
##  CREATING QUASIGROUPS AND LOOPS FROM A FILE
##  -------------------------------------------------------------------------

#############################################################################
##  
#F  ReadCayleyTableFromFile( filename, replace_by_spaces ) 
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

ReadCayleyTableFromFile := function( filename, replace_by_spaces)

    local input, s, i, chunks, started, starting_pos, z, j, distinct_chunks, c, n, T;
    
    if not ( IsString( filename) and IsString( replace_by_spaces ) ) then
        Error( "LOOPS: <1> must be a file name, and <2> must be a string." );
    fi;
      
    input := InputTextFile( filename );
    if input = fail then Error( "LOOPS: <1> is not a valid file name." ); fi;

    s := ReadAll( input );

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

end;

#############################################################################
##  
#O  QuasigroupFromFile( filename, replace] ) 
##    
##  Calls ReadCayleyTableFromFile( filename, replace ) in order to return
##  the quasigroup with multiplication table in file <filename>.

InstallMethod( QuasigroupFromFile, "for string and string",
    [ IsString, IsString ],
function( filename, replace )
    return QuasigroupByCayleyTable( ReadCayleyTableFromFile( filename, replace ) );
end );


#############################################################################
##  
#O  LoopFromFile( filename , replace] ) 
##    
##  Calls ReadCayleyTableFromFile( filename, replace ) in order to return
##  the loop with multiplication table in file <filename>.

InstallMethod( LoopFromFile, "for string and string",
    [ IsString, IsString ],
function( filename, replace )
    return LoopByCayleyTable(  ReadCayleyTableFromFile( filename, replace ) );
end );

#############################################################################
##  CONVERSIONS
##  -------------------------------------------------------------------------

#############################################################################
##  
#O  AsQuasigroup( M ) 
##    
##  Given a magma, returns the corresponding quasigroup, if possible.

InstallMethod( AsQuasigroup, "for magma",
    [ IsMagma ],
function( M )
    local ct;
    if IsQuasigroup( M ) then return M; fi;
    if IsGroup( M ) then 
        return QuasigroupByCayleyTable( MultiplicationTable( Elements( M ) ) );
    fi;
    # magma, not a quasigroup
    ct := MultiplicationTable( Elements( M ) );
    if IsQuasigroupTable( ct ) then return QuasigroupByCayleyTable( ct ); fi;
    return fail;
end);

#############################################################################
##  
#O  PrincipalLoopIsotope( Q, f, g ) 
##    
##  Given a quasigroup Q and elements f, g of Q, returns the principal
##  loop isotope of Q with respect to f, g.

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
    
    # must make sure that the neutral element f*g is labelled as 1
    p := Position( Q, f*g );
    if p > 1 then #renaming the elements inside the table
        for i in [1..n] do for j in [1..n] do 
            if ct[ i ][ j ] = 1 then ct[ i ][ j ] := p;
            elif ct[ i ][ j ] = p then ct[ i ][ j ] := 1;
            fi;
        od; od;
    fi;
    return LoopByCayleyTable( NormalizedQuasigroupTable( ct ) );
end);    

#############################################################################
##  
#O  AsLoop( M ) 
##    
##  Given a magma, returns the corresponding loop, if possible.

InstallMethod( AsLoop, "for magma",
    [ IsMagma ],
function( M )
    local n, e, p, ct, new_ct, i, j;
    if IsLoop( M ) then return M; fi;
    if IsGroup( M ) then 
        return LoopByCayleyTable( MultiplicationTable( Elements( M ) ) );
    fi;
    # magma, not a loop
    M := AsQuasigroup( M );
    if M = fail then return fail; fi;
    # quasigroup
    e := MultiplicativeNeutralElement( M );
    if e = fail then # no neutral element
        return PrincipalLoopIsotope( M, Elements( M )[ 1 ], Elements( M )[ 1 ] );
    fi;
    # quasigroup with neutral element
    p := Position( M, e );
    ct := CayleyTable( M );
    if p = 1 then return LoopByCayleyTable( ct ); fi;
    # p>1
    p := (1,p); #isomorphism 
    n := Size( M );
    new_ct := List( [1..n] , i->[1..n]);
    for i in [1..n] do for j in [1..n] do
        # isomorphic copy via p. Note that p = p^-1
        new_ct[ i ][ j ] := (ct[ i^p ][ j^  p ])^p;
    od; od;
    return LoopByCayleyTable( new_ct );
end );
 
#############################################################################
##  
#A  AsGroup( M ) 
##    
##  Given a magma, returns the corresponding group, if possible.

InstallOtherMethod( AsGroup, 
    [ IsMagma ],
function( M )
    if IsGroup( M ) then return M; fi;
    M := AsLoop( M );
    if (not M = fail) and IsAssociative( M ) then 
        return RightMultiplicationGroup( M ); 
    fi;
    return fail;
end );

#############################################################################
##  PRODUCTS OF LOOPS
##  -------------------------------------------------------------------------

#############################################################################
##  
#F  DirectProduct( L1, L2, ..., Ln ) 
##    
##  Returns the direct product of loops <L1>, <L2>, ... , <Ln>. The loops can
##  be declared as loops or as groups.

# The following is necessary due to implementation of DirectProduct for 
# groups in GAP. The idea is as follows:
# We want to calculate direct product of loops and groups.
# If only groups are on the list, standard GAP DirectProduct will take care
# of it. If there are also some loops on the list, WE must take care of it.
# However, we do not know if such a list will be processed with
# DirectProductOp( <IsList>, <IsGroup> ) or with
# DirectProductOp( <IsList>, <IsLoop>), since this depends on whether
# a group or a loop is listed first. We therefore take care of both
# situations.

InstallOtherMethod( DirectProductOp, "for DirectProduct( <IsList>, <IsGroup> )",
    [ IsList, IsGroup],
function( list, first )
    local L, p;
    
    # Check the arguments.
    if IsEmpty( list ) then Error( "LOOPS: <arg 1> must be nonempty." ); fi;
    if not ForAny( list, IsLoop ) then 
    # there are no loops on the list
    TryNextMethod(); 
    fi;
    if ForAny( list, G -> (not IsGroup( G )) and (not IsLoop( G ) ) ) then
    # there are other objects beside groups and loops on the list
        TryNextMethod(); 
    fi;
    
    # all arguments are groups or loops, and there is at least one loop
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

InstallOtherMethod( DirectProductOp, "for DirectProduct( <IsList>, <IsLoop> )",
    [ IsList, IsLoop ],
function( list, dummy )

    local group_list, loop_list, group_product, n, i, nL, nM, TL, TM, T, j, k, s;
    
    # Check the arguments.
    if IsEmpty( list ) then
      Error( "LOOPS: <1> must be nonempty." );
    elif ForAny( list, G -> (not IsGroup( G )) and (not IsLoop( G ) ) ) then
      TryNextMethod();
    fi;
    
    # only groups and loops are on the list, and there is at least one loop
    group_list := Filtered( list, G -> IsGroup( G ) );
    loop_list := Filtered( list, G -> IsLoop( G ) ); 
    if not IsEmpty( group_list ) then   # some groups on the list 
        group_product := DirectProductOp( group_list, group_list[ 1 ] );
        Add( loop_list, AsLoop( group_product ) );
    fi;
    
    # now only loops are on the list
    n := Length( loop_list );
    if n=1 then return loop_list[ 1 ]; fi;
    # at least 2 loops, don't want to use recursion
    for s in [2..n] do
        nL := Size( loop_list[ 1 ] );
        nM := Size( loop_list[ s ] );
        TL := CayleyTable( loop_list[ 1 ] );
        TM := CayleyTable( loop_list[ s ] );
        T := List( [1..nL*nM], j->[] );

        # not efficient, but it does the job
        for i in [1..nM] do for j in [1..nM] do for k in [1..nL] do
            Append( T[ (i-1)*nL + k ], TL[ k ] + nL*(TM[i][j]-1) );
        od; od; od;
        loop_list[ 1 ] := LoopByCayleyTable( T );
    od;     
    return loop_list[ 1 ];
 end );

#############################################################################
## OPPOSITE QUASIGROUPS AND LOOPS
## --------------------------------------------------------------------------

#############################################################################
##  
#O  Opposite( Q ) 
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

## y = LeftInverse( x ) if yx = 1. Note (1/x)x = 1.
InstallMethod( LeftInverse, "for loop elements",
    [ IsLoopElement ],
    x -> RightDivision( One( x ), x ) 
);

## y = RightInverse( x ) if xy = 1. Note x(1\x) = 1.
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
    if HasIsAssociative( L ) and IsAssociative( L ) then
        Print( "<associative loop of order ", Size( L ), ">");
    elif HasIsExtraLoop( L ) and IsExtraLoop( L ) then
        Print( "<extra loop of order ", Size( L ), ">");
    elif HasIsMoufangLoop( L ) and IsMoufangLoop( L ) then
        Print( "<Moufang loop of order ", Size( L ), ">");
    elif HasIsCLoop( L ) and IsCLoop( L ) then
        Print( "<C loop of order ", Size( L ), ">"); 
    elif HasIsLeftBolLoop( L ) and IsLeftBolLoop( L ) then
        Print( "<left Bol loop of order ", Size( L ), ">");
    elif HasIsRightBolLoop( L ) and IsRightBolLoop( L ) then
        Print( "<right Bol loop of order ", Size( L ), ">");
    elif HasIsLCLoop( L ) and IsLCLoop( L ) then
        Print( "<LC loop of order ", Size( L ), ">");
    elif HasIsRCLoop( L ) and IsRCLoop( L ) then
        Print( "<RC loop of order ", Size( L ), ">");
    elif HasIsLeftAlternative( L ) and IsLeftAlternative( L ) then
        if HasIsRightAlternative( L ) and IsRightAlternative( L ) then
            Print( "<alternative loop of order ", Size( L ), ">");
        else
            Print( "<left alternative loop of order ", Size( L ), ">");
        fi;
    elif HasIsRightAlternative( L ) and IsRightAlternative( L ) then
        Print( "<right alternative loop of order ", Size( L ), ">");
    elif HasIsFlexible( L ) and IsFlexible( L ) then
        Print( "<flexible loop of order ", Size( L ), ">");
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

#############################################################################
##  BASIC ATTRIBUTES
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
##  GENERATORS
##  -------------------------------------------------------------------------

#############################################################################
##  
#A  GeneratorsOfQuasigroup( Q ) 
##    
##  Returns a subset of <Q> gnerating <Q>. In most circumstances, 
##  Elements( Q ) is returned.

InstallMethod( GeneratorsOfQuasigroup, "for quasigroup",
    [ IsQuasigroup ],
    GeneratorsOfDomain 
);

#############################################################################   
##
#A  GeneratorsSmallest( Q )     
##
##  Returns a small generating set of quasigroup <Q>. When <Q> has order n,     
##  the set has size at most log_2( n ).
  
InstallOtherMethod( GeneratorsSmallest, "for a quasigroup",  
        [ IsQuasigroup ],     
        function( Q ) 
    local gens, diff;     
    gens := [ ];    
    diff := Elements( Q );
    while diff <> [ ] do  
        Add( gens, diff[ Length(diff) ] ); 
        if IsLoop( Q ) then 
            diff := Difference( diff, Subloop( Q, gens ) ); 
        else
            diff := Difference( diff, Subquasigroup( Q, gens ) ); 
        fi;
    od; 
    return Set( gens );   
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
##    b) pos_of_gens is a subset of [ 1..Size( Q ) ] (not a sublist)
##    c) pos_of_gens determines a subquasigroup of Q
##  It then returns the corresponding subquasigroup of Q.

InstallMethod( SubquasigroupNC, "for quasigroup and subset of integer",
    [ IsQuasigroup, IsSet ],
function( Q, pos_of_gens )
    local subqg, Qtype, elms;
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
    if IsEmpty( gens ) then 
        Error( "LOOPS: the list of generators of a quasigroup cannot be empty" ); 
    fi;
    if not( ForAll( gens, g -> g in Q ) or ForAll( gens, g-> g in PosInParent( Q ) ) ) then 
        Error("LOOPS: <2> must be a list of elements of quasigroup <1> or their indices."); 
    fi;
    if not IsInt( gens[1] ) then 
        pos_gens := PosInParent( gens );
    else
        pos_gens := ShallowCopy( gens );
        gens := Elements( Parent( Q ) ){ gens };
    fi;
    transl := Union( LeftSection( Parent( Q ) ){ pos_gens }, 
        RightSection( Parent( Q ) ){ pos_gens } );
    relmultgr := Subgroup( MultiplicationGroup( Parent( Q ) ), transl );
    pos_of_elms := Set( Orbits( relmultgr, pos_gens )[ 1 ] );
    subqg := SubquasigroupNC( Parent( Q ), pos_of_elms );
    if IsLoop( Q ) and Size( Q ) > 1 then 
        SetGeneratorsOfMagma( subqg, Difference( gens, [ One( Q ) ] ) );
    else
        SetGeneratorsOfMagma( subqg, gens );
    fi;
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
    local sl, pos_of_gens;
    # NG: another small change
    if gens<>[] and ( gens[1] in L ) then
        pos_of_gens := Union( [1], PosInParent( gens ) );
    else
        pos_of_gens := Union( [1], gens );
    fi;
    sl := Subquasigroup( L, pos_of_gens );
    SetOne( sl, One( L ) );
    return sl;
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
    if IsLoop( Q ) then return Subloop( Q, Elements( Q ){ S } ); fi;
    return Subquasigroup( Q, Elements( Q ){ S } );
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
    if IsLoop( Q ) then return Subloop( Q, Elements( Q ){ S } ); fi;
    return Subquasigroup( Q, Elements( Q ){ S } );    
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
    if IsLoop( Q ) then return Subloop( Q, Elements( Q ){ S } ); fi;
    return Subquasigroup( Q, Elements( Q ){ S } );
end);

#############################################################################
##  
#A  Nuc( Q ) 
##     
##  The nucleus of quasigroup <Q>.

InstallMethod( Nuc, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local N;
    N := Intersection( Elements( LeftNucleus( Q ) ), 
        Elements( RightNucleus( Q ) ), Elements( MiddleNucleus( Q ) ) );
    if IsLoop( Q ) then return Subloop( Q, N ); fi;
    return Subquasigroup( Q, N );
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
    local S, L;
    S := Intersection( Nuc( Q ), Commutant( Q ) );
    if IsLoop( Q ) then 
        L := Subloop( Q, S );
        SetIsAssociative( L, true );
    else
        L := Subquasigroup( Q, S );
    fi;
    SetIsCommutative( L, true );
    return L;
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
    # BETTER ALGORITHM LATER?
    local x, y, z, A;
    A := [];
    for x in L do for y in L do for z in L do
        AddSet( A, Associator( x, y, z ) );
    od; od; od;
    return Subloop( L, A );
end);

#############################################################################
##  ASSOCIATIVITY, COMMUTATIVITY AND GENERALIZATIONS
##  -------------------------------------------------------------------------

# IsAssociative already implemented for magmas.

# implies
InstallTrueMethod( IsExtraLoop, IsAssociative and IsLoop );
InstallTrueMethod( IsDiassociative, IsAssociative and IsLoop );

#############################################################################
##  
#P  IsCommutative( Q ) 
##     
##  Returns true if <Q> is commutative.

InstallOtherMethod( IsCommutative, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    return LeftSection( Q ) = RightSection( Q );
end );

# implies (left property <-> right property)
InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and IsCommutative );
InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and IsCommutative );
InstallTrueMethod( IsMoufangLoop, IsRightBolLoop and IsCommutative );
InstallTrueMethod( IsMoufangLoop, IsLeftBolLoop and IsCommutative );
InstallTrueMethod( IsMoufangLoop, IsRightBruckLoop and IsCommutative );
InstallTrueMethod( IsMoufangLoop, IsLeftBruckLoop and IsCommutative );
InstallTrueMethod( IsRightNuclearSquareLoop, IsLeftNuclearSquareLoop and IsCommutative );
InstallTrueMethod( IsLeftNuclearSquareLoop, IsRightNuclearSquareLoop and IsCommutative );
InstallTrueMethod( HasAutomorphicInverseProperty, HasAntiautomorphicInverseProperty and IsCommutative );
InstallTrueMethod( HasAntiautomorphicInverseProperty, HasAutomorphicInverseProperty and IsCommutative );
InstallTrueMethod( IsAlternative, IsLeftAlternative and IsCommutative );
InstallTrueMethod( IsAlternative, IsRightAlternative and IsCommutative );

#############################################################################
##  
#P  IsPowerAssociative( L ) 
##     
##  Returns true if <L> is a power associative loop.

InstallOtherMethod( IsPowerAssociative, "for loop",
    [ IsLoop ],
function( L )
    return ForAll( L, x -> IsAssociative( Subloop( L, [ x ] ) ) );
end );

# implies
InstallTrueMethod( HasTwosidedInverses, IsPowerAssociative );

#############################################################################
##  
#P  IsDiassociative( L )
##     
##  Returns true if <L> is a diassociative loop.

InstallOtherMethod( IsDiassociative, "for loop",
    [ IsLoop ],
function( L )
    # BETTER ALGORITHM LATER
    return ForAll( L, x -> ForAll( L, y -> 
        IsAssociative( Subloop( L, [ x, y ] ) ) ) );
end );

# implies
InstallTrueMethod( IsPowerAssociative, IsDiassociative );
InstallTrueMethod( IsAlternative, IsDiassociative );
InstallTrueMethod( IsFlexible, IsDiassociative );

#############################################################################
##  INVERSE PROPERTIES
##  -------------------------------------------------------------------------

#############################################################################
##  
#P  HasLeftInverseProperty( L ) 
##     
##  Returns true if <L> has the left inverse property.

InstallMethod( HasLeftInverseProperty, "for loop", 
    [ IsLoop ],
function( L )
    return ForAll( LeftSection( L ), a -> a^-1 in LeftSection( L ) );
end );

#############################################################################
##  
#P  HasRightInverseProperty( L ) 
##     
##  Returns true if <L> has the right inverse property.

InstallMethod( HasRightInverseProperty, "for loop", 
    [ IsLoop ],
function( L )
    return ForAll( RightSection( L ), a -> a^-1 in RightSection( L ) );
end );

#############################################################################
##  
#P  HasInverseProperty( L ) 
##     
##  Returns true if <L> has the inverse property.

InstallMethod( HasInverseProperty, "for loop", 
    [ IsLoop ],
function( L )
    return HasLeftInverseProperty( L ) and HasRightInverseProperty( L );
end );

#############################################################################
##  
#P  HasWeakInverseProperty( L ) 
##     
##  Returns true if <L> has the weak inverse property.

InstallMethod( HasWeakInverseProperty, "for loop", 
    [ IsLoop ],
function( L )
    return ForAll( L, x -> ForAll( L, y -> LeftInverse(x*y)*x=LeftInverse(y) ));
end );

#############################################################################
##  
#P  HasTwosidedInverses( L ) 
##     
##  Returns true if <L> has two-sided inverses.

InstallMethod( HasTwosidedInverses, "for loop", 
    [ IsLoop ],
function( L )
    return ForAll( L, x -> LeftInverse( x ) = RightInverse( x ) );
end );

#############################################################################
##  
#P  HasAutomorphicInverseProperty( L ) 
##     
##  Returns true if <L> has the automorphic inverse property.

InstallMethod( HasAutomorphicInverseProperty, "for loop", 
    [ IsLoop ],
function( L )
    return ForAll( L, x -> ForAll( L, y -> 
        LeftInverse( x*y ) = LeftInverse( x )*LeftInverse( y ) ) );
end );

#############################################################################
##  
#P  HasAntiautomorphicInverseProperty( L ) 
##     
##  Returns true if <L> has the antiautomorphic inverse property.

InstallMethod( HasAntiautomorphicInverseProperty, "for loop", 
    [ IsLoop ], 
function( L )
    return ForAll( L, x -> ForAll( L, y -> 
        LeftInverse( x*y ) = LeftInverse( y )*LeftInverse( y ) ) );
end );

# implies and is implied by (for inverse properties)
InstallTrueMethod( HasLeftInverseProperty, HasInverseProperty );
InstallTrueMethod( HasRightInverseProperty, HasInverseProperty );
InstallTrueMethod( HasWeakInverseProperty, HasInverseProperty );
InstallTrueMethod( HasAntiautomorphicInverseProperty, HasInverseProperty );
InstallTrueMethod( HasTwosidedInverses, HasAntiautomorphicInverseProperty );
InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasRightInverseProperty );
InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasWeakInverseProperty );
InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and HasWeakInverseProperty );
InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasAntiautomorphicInverseProperty );
InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and HasAntiautomorphicInverseProperty );
InstallTrueMethod( HasInverseProperty, HasWeakInverseProperty and HasAntiautomorphicInverseProperty );


#############################################################################
##  PROPERTIES OF QUASIGROUPS
##  -------------------------------------------------------------------------

#############################################################################
##  
#P  IsSemisymmetric( Q ) 
##     
##  Returns true if the quasigroup <Q> is semisymmetric, i.e., (xy)x=y.

InstallMethod( IsSemisymmetric, "for quasigroup", 
    [ IsQuasigroup ],
function( Q )
    return ForAll( Q, x -> 
        LeftTranslation( Q, x ) * RightTranslation( Q, x ) = () );
end );

#############################################################################
##  
#P  IsTotallySymmetric( Q ) 
##     
##  Returns true if the quasigroup <Q> is totally symmetric, i.e, 
##  commutative and semisymmetric.

InstallMethod( IsTotallySymmetric, "for quasigroup", 
    [ IsQuasigroup ],
function( Q )
    return IsCommutative( Q ) and IsSemisymmetric( Q );
end );

#############################################################################
##  
#P  IsIdempotent( Q ) 
##     
##  Returns true if the quasigroup <Q> is idempotent, i.e., x*x=x.

InstallMethod( IsIdempotent, "for quasigroup", 
    [ IsQuasigroup ],
function( Q )
    return ForAll( Q, x -> x*x = x );
end );

#############################################################################
##  
#P  IsSteinerQuasigroup( Q ) 
##     
##  Returns true if the quasigroup <Q> is a Steiner quasigroup, i.e.,
##  idempotent and totally symmetric.

InstallMethod( IsSteinerQuasigroup, "for quasigroup", 
    [ IsQuasigroup ],
function( Q )
    return IsIdempotent( Q ) and IsTotallySymmetric( Q );
end );

#############################################################################
##  
#P  IsUnipotent( Q ) 
##     
##  Returns true if the quasigroup <Q> is unipotent, i.e., x*x=y*y.

InstallMethod( IsUnipotent, "for quasigroup", 
    [ IsQuasigroup ],
function( Q )
    local square;
    if IsLoop( Q ) then return IsIdempotent( Q ); fi;
    square :=  Elements( Q )[ 1 ]^2;
    return ForAll( Q, y -> y^2 = square );
end );

#############################################################################
##  
#P  IsLDistributive( Q ) 
##     
##  Returns true if the quasigroup <Q> is left distributive.

InstallOtherMethod( IsLDistributive, "for Quasigroup", 
    [ IsQuasigroup ],
function( Q )
    # BETTER ALGORITHM LATER?
    local x, y, z;
    for x in Q do for y in Q do for z in Q do
        if not x*(y*z) = (x*y)*(x*z) then return false; fi;
    od; od; od;
    return true;
end );

#############################################################################
##  
#P  IsRDistributive( Q ) 
##     
##  Returns true if the quasigroup <Q> is right distributive.

InstallOtherMethod( IsRDistributive, "for Quasigroup", 
    [ IsQuasigroup ],
function( Q )
    # BETTER ALGORITHM LATER?
    local x, y, z;
    for x in Q do for y in Q do for z in Q do
        if not (x*y)*z = (x*z)*(y*z) then return false; fi;
    od; od; od;
    return true;
end );

#############################################################################
##  
#P  IsEntropic( Q ) 
##     
##  Returns true if the quasigroup <Q> is entropic.

InstallMethod( IsEntropic, "for quasigroup", 
    [ IsQuasigroup ],
function( Q )
    # BETTER ALGORITHM LATER?
    local x, y, z, w;
    for x in Q do for y in Q do for z in Q do for w in Q do
        if not (x*y)*(z*w) = (x*z)*(y*w) then return false; fi;
    od; od; od; od;
    return true;
end );

#############################################################################
##  LOOPS OF BOL-MOUFANG TYPE AND RELATED PROPERTIES
##  -------------------------------------------------------------------------

#############################################################################
##  
#P  IsExtraLoop( L ) 
##     
##  Returns true if <L> is an extra loop.

InstallMethod( IsExtraLoop, "for loop",
    [ IsLoop ],
function( L )
    return IsMoufangLoop( L ) and IsNuclearSquareLoop( L );
end );

# implies
InstallTrueMethod( IsMoufangLoop, IsExtraLoop );
InstallTrueMethod( IsNuclearSquareLoop, IsExtraLoop );
InstallTrueMethod( IsCLoop, IsExtraLoop );

# is implied by
InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsLeftNuclearSquareLoop );
InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsMiddleNuclearSquareLoop );
InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsRightNuclearSquareLoop );

#############################################################################
##  
#P  IsMoufangLoop( L ) 
##     
##  Returns true if <L> is a Moufang loop.

InstallMethod( IsMoufangLoop, "for loop", 
    [ IsLoop ],
function( L )
    return IsLeftBolLoop( L ) and HasRightInverseProperty( L );
end );

# implies
InstallTrueMethod( IsLeftBolLoop, IsMoufangLoop );
InstallTrueMethod( IsRightBolLoop, IsMoufangLoop );
InstallTrueMethod( IsFlexible, IsMoufangLoop );
InstallTrueMethod( IsDiassociative, IsMoufangLoop );

# is implied by
InstallTrueMethod( IsMoufangLoop, IsLeftBolLoop and IsRightBolLoop );

#############################################################################
##  
#P  IsCLoop( L ) 
##     
##  Returns true if <L> is a C-loop.

InstallMethod( IsCLoop, "for loop", 
    [ IsLoop ],
function( L )
    return IsLCLoop( L ) and IsRCLoop( L );
end );

# implies
InstallTrueMethod( IsLCLoop, IsCLoop );
InstallTrueMethod( IsRCLoop, IsCLoop );

# is implied by
InstallTrueMethod( IsCLoop, IsLCLoop and IsRCLoop );

#############################################################################
##  
#P  IsLeftBolLoop( L ) 
##     
##  Returns true if <L> is a left Bol loop.

InstallMethod( IsLeftBolLoop, "for loop", 
    [ IsLoop ],
function( L )
    return ForAll( LeftSection( L ), a -> 
        ForAll( LeftSection( L ), b -> a*b*a in LeftSection( L ) ) );
end );

# implies
InstallTrueMethod( IsLeftAlternative, IsLeftBolLoop );
InstallTrueMethod( HasTwosidedInverses, IsLeftBolLoop );

#############################################################################
##  
#P  IsRightBolLoop( L ) 
##     
##  Returns true if <L> is a right Bol loop.

InstallMethod( IsRightBolLoop, "for loop", 
    [ IsLoop ],
function( L )
    return ForAll( RightSection( L ), a -> 
        ForAll( RightSection( L ), b -> a*b*a in RightSection( L ) ) );
end );

# implies
InstallTrueMethod( IsRightAlternative, IsRightBolLoop );
InstallTrueMethod( HasTwosidedInverses, IsRightBolLoop );

#############################################################################
##  
#P  IsLCLoop( L ) 
##     
##  Returns true if <L> is an LC-loop.

InstallMethod( IsLCLoop, "for loop", 
    [ IsLoop ],
function( L )
    return ForAll( LeftSection( L ), a ->
        ForAll( RightSection( L ), b -> b^(-1)*a*a*b in LeftSection( L ) ) );
end );

# implies
InstallTrueMethod( IsLeftAlternative, IsLCLoop );
InstallTrueMethod( IsLeftNuclearSquareLoop, IsLCLoop );
InstallTrueMethod( IsMiddleNuclearSquareLoop, IsLCLoop );
InstallTrueMethod( IsPowerAssociative, IsLCLoop );

#############################################################################
##  
#P  IsRCLoop( L ) 
##     
##  Returns true if <L> is an RC-loop.

InstallMethod( IsRCLoop, "for loop", 
    [ IsLoop ],
function( L )
    return ForAll( LeftSection( L ), a ->
        ForAll( RightSection( L ), b -> a^(-1)*b*b*a in RightSection( L ) ) );
end );

# implies
InstallTrueMethod( IsRightAlternative, IsRCLoop );
InstallTrueMethod( IsRightNuclearSquareLoop, IsRCLoop );
InstallTrueMethod( IsMiddleNuclearSquareLoop, IsRCLoop );
InstallTrueMethod( IsPowerAssociative, IsRCLoop );

#############################################################################
##  
#P  IsLeftNuclearSquareLoop( L ) 
##     
##  Returns true if <L> is a left nuclear square loop.

InstallMethod( IsLeftNuclearSquareLoop, "for loop", 
    [ IsLoop ],
function( L )
    return ForAll( L, x -> x^2 in LeftNucleus( L ) );
end );

#############################################################################
##  
#P  IsMiddleNuclearSquareLoop( L ) 
##     
##  Returns true if <L> is a middle nuclear square loop.

InstallMethod( IsMiddleNuclearSquareLoop, "for loop", 
    [ IsLoop ],
function( L )
    return ForAll( L, x -> x^2 in MiddleNucleus( L ) );
end );

#############################################################################
##  
#P  IsRightNuclearSquareLoop( L ) 
##     
##  Returns true if <L> is a right nuclear square loop.

InstallMethod( IsRightNuclearSquareLoop, "for loop", 
    [ IsLoop ],
function( L )
    return ForAll( L, x -> x^2 in RightNucleus( L ) );
end );

#############################################################################
##  
#P  IsNuclearSquareLoop( L ) 
##     
##  Returns true if <L> is a nuclear square loop.

InstallMethod( IsNuclearSquareLoop, "for loop",
    [ IsLoop ],
function( L )
    return IsLeftNuclearSquareLoop( L ) and IsRightNuclearSquareLoop( L )
        and IsMiddleNuclearSquareLoop( L );
end );

# implies
InstallTrueMethod( IsLeftNuclearSquareLoop, IsNuclearSquareLoop );
InstallTrueMethod( IsRightNuclearSquareLoop, IsNuclearSquareLoop );
InstallTrueMethod( IsMiddleNuclearSquareLoop, IsNuclearSquareLoop );

# is implied by
InstallTrueMethod( IsNuclearSquareLoop, IsLeftNuclearSquareLoop 
    and IsRightNuclearSquareLoop and IsMiddleNuclearSquareLoop );

#############################################################################
##  
#P  IsFlexible( Q ) 
##     
##  Returns true if <Q> is a flexible quasigroup.

InstallMethod( IsFlexible, "for quasigroup", 
    [ IsQuasigroup ],
function( Q )
    local LS, RS;
    LS := LeftSection( Q );
    RS := RightSection( Q );
    return ForAll( [1..Size( Q )], i -> LS[ i ] * RS[ i ] = RS[ i ] * LS[ i ] );
end );

#############################################################################
##  
#P  IsLeftAlternative( Q ) 
##     
##  Returns true if <Q> is a left alternative quasigroup.

InstallMethod( IsLeftAlternative, "for quasigroup", 
    [ IsQuasigroup],
function( Q )
   return ForAll( LeftSection( Q ), a -> a*a in LeftSection( Q ) );
end );

#############################################################################
##  
#P  IsRightAlternative( Q ) 
##     
##  Returns true if <Q> is a right alternative quasigroup.

InstallMethod( IsRightAlternative, "for quasigroup", 
    [ IsQuasigroup ],
function( Q )
    return ForAll( RightSection( Q ), a -> a*a in RightSection( Q ) );
end );

#############################################################################
##  
#P  IsAlternative( Q ) 
##     
##  Returns true if <Q> is an alternative quasigroup.

InstallMethod( IsAlternative, "for quasigroup", 
    [ IsQuasigroup ],
function( Q )
    return IsLeftAlternative( Q ) and IsRightAlternative( Q );
end );

# implies
InstallTrueMethod( IsLeftAlternative, IsAlternative );
InstallTrueMethod( IsRightAlternative, IsAlternative );

# is implied by
InstallTrueMethod( IsAlternative, IsLeftAlternative and IsRightAlternative );

#############################################################################
##  CC-LOOPS AND RELATED PROPERTIES
##  -------------------------------------------------------------------------

#############################################################################
##  
#P  IsLCCLoop( L ) 
##     
##  Returns true if <L> is a left conjugacy closed loop.

InstallMethod( IsLCCLoop, "for loop",
    [ IsLoop ],
function( L )
    return ForAll( LeftSection( L ), a -> 
        ForAll( LeftSection( L ), b -> b*a*b^(-1) in LeftSection( L ) ) );
end );

#############################################################################
##  
#P  IsRCCLoop( L ) 
##     
##  Returns true if <L> is a right conjugacy closed loop.

InstallMethod( IsRCCLoop, "for loop",
    [ IsLoop ],
function( L )
    return ForAll( RightSection( L ), a -> 
        ForAll( RightSection( L ), b -> b*a*b^(-1) in RightSection( L ) ) );
end );

#############################################################################
##  
#P  IsCCLoop( L ) 
##     
##  Returns true if <L> is a conjugacy closed loop.

InstallMethod( IsCCLoop, "for loop",
    [ IsLoop ],
function( L )
    return IsLCCLoop( L ) and IsRCCLoop( L );
end );

# implies
InstallTrueMethod( IsLCCLoop, IsCCLoop );
InstallTrueMethod( IsRCCLoop, IsCCLoop );

# is implied by
InstallTrueMethod( IsCCLoop, IsLCCLoop and IsRCCLoop );

#############################################################################
##  
#P  IsOsbornLoop( L ) 
##     
##  Returns true if <L> is an Osborn loop.

InstallMethod( IsOsbornLoop, "for loop", 
    [ IsLoop ],
function( L )
    # MIGHT REDO LATER
    return ForAll(L, x-> ForAll(L, y->   
        ForAll(L, z-> x*((y*z)*x) = LeftDivision(LeftInverse(x),y)*(z*x))
    ));
end );
 
# is implied by
InstallTrueMethod( IsOsbornLoop, IsMoufangLoop );
InstallTrueMethod( IsOsbornLoop, IsCCLoop );

#############################################################################
##  ADDITIONAL VARIETIES OF LOOPS
##  -------------------------------------------------------------------------

#############################################################################
##  
#P  IsLeftBruckLoop( L ) 
##     
##  Returns true if <L> is a left Bruck loop.

InstallMethod( IsLeftBruckLoop, "for loop",
    [ IsLoop ],
function( L )
    return IsLeftBolLoop( L ) and HasAutomorphicInverseProperty( L );
end );

# implies 
InstallTrueMethod( HasAutomorphicInverseProperty, IsLeftBruckLoop );
InstallTrueMethod( IsLeftBolLoop, IsLeftBruckLoop );

# is implied by
InstallTrueMethod( IsLeftBruckLoop, IsLeftBolLoop and HasAutomorphicInverseProperty );

#############################################################################
##  
#P  IsRightBruckLoop( L ) 
##     
##  Returns true if <L> is a right Bruck loop.

InstallMethod( IsRightBruckLoop, "for loop",
    [ IsLoop ],
function( L )
    return IsRightBolLoop( L ) and HasAutomorphicInverseProperty( L );
end );

# implies 
InstallTrueMethod( HasAutomorphicInverseProperty, IsRightBruckLoop );
InstallTrueMethod( IsRightBolLoop, IsRightBruckLoop );

# is implied by
InstallTrueMethod( IsRightBruckLoop, IsRightBolLoop and HasAutomorphicInverseProperty );

#############################################################################
##  
#P  IsSteinerLoop( L ) 
##     
##  Returns true if <L> is a Steiner loop.

InstallMethod( IsSteinerLoop, "for loop",
    [ IsLoop ],
function( L )
    # Steiner loops are inverse property loops of exponent at most 2.
    return HasInverseProperty( L ) and Exponent( L )<=2;
end );

# implies
InstallTrueMethod( IsCommutative, IsSteinerLoop );
InstallTrueMethod( HasInverseProperty, IsSteinerLoop );

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
##  Returs the upper central series of the loop L. 

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
##  Returs the lower central series of the loop L. 

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
