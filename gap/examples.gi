#############################################################################
##
#W  examples.gi              Examples of loops   G. P. Nagy / P. Vojtechovsky
##  
#H  @(#)$Id: examples.gi, v 0.9995 2005/07/22 gap Exp $
##  
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  READING DATA
##  -------------------------------------------------------------------------

ReadPkg("loops", "data/leftbol.tbl");       # left Bol loops
ReadPkg("loops", "data/moufang.tbl");       # Moufang loops
ReadPkg("loops", "data/paige.tbl");         # Paige loops
ReadPkg("loops", "data/steiner.tbl");       # Steiner loops
ReadPkg("loops", "data/cc.tbl");            # CC-loops
ReadPkg("loops", "data/interesting.tbl");   # interesting loops

#############################################################################
##  DISPLAYING INFORMATION ABOUT A LIBRARY
##  -------------------------------------------------------------------------

#############################################################################
##  
#F  LibraryByName( name ) 
##    
##  Auxiliary. Returns the library corresponding to <name>.

LibraryByName := function( name )
    if name = "left Bol" then return left_bol_data; 
    elif name = "Moufang" then return moufang_data;
    elif name = "Paige" then return paige_data;
    elif name = "Steiner" then return steiner_data;
    elif name = "CC" then return cc_data;
    elif name = "interesting" then return interesting_data;
    fi;
end;

#############################################################################
##  
#F  DisplayLibraryInfo( name ) 
##    
##  Display information about library named <name>.

InstallGlobalFunction( DisplayLibraryInfo, function( name )
    local s, lib, k;
    if name = "left Bol" then
        s := "The library contains all left Bol loops of order 8 \nthat are not Moufang.";
    elif name = "Moufang" then
        s := "The library contains all nonassociative Moufang loops \nof order less than 64, and 4262 nonassociative Moufang \nloops of order 64.";
    elif name = "Paige" then
        s := "The library contains the smallest nonassociative finite \nsimple Moufang loop.";
    elif name = "Steiner" then
        s := "The library contains all nonassociative Steiner loops \nof order less or equal to 16. It also contains the \nassociative Steiner loops of order 4 and 8.";
    elif name = "CC" then
        s := "The library contains all nonassociative CC-loops \nof order p^2 and 2*p for any odd prime p.\nThere are precisely 3 such loops of order p^2,\nand precisely 1 such loop of order 2*p.";
    elif name = "interesting" then
        s := "The library contains a few interesting loops.";
    else
    Error( 
        Concatenation(
        "The admissible names for loop libraries are: \n",
        "[ \"left Bol\", \"Moufang\", \"Paige\", \"Steiner\", \"CC\", ",
        "\"interesting\" ]."
        )
        );
    fi;
    
    s := Concatenation( s, "\n------\nExtent of the library:" );
    
    lib := LibraryByName( name );
    if not name = "CC" then
        for k in [1..Length( lib[ 1 ] ) ] do
            if lib[ 2 ][ k ] = 1 then
                s := Concatenation( s, "\n   ", String( lib[ 2 ][ k ] ), " loop of order ", String( lib[ 1 ][ k ] ) );
            else
                s := Concatenation( s, "\n   ", String( lib[ 2 ][ k ] ), " loops of order ", String( lib[ 1 ][ k ] ) );
            fi;
        od;
    else
        s := Concatenation( s, "\n 3 loops of order p^2 and 1 loop of order 2*p\n for every odd prime p" );
    fi;     
    s := Concatenation( s, "\n" );
    Print( s );
    return true;
end);

#############################################################################
##  AUXILIARY FUNCTIONS
##  -------------------------------------------------------------------------
##  When the data in the database is encoded in some way, we usually
##  retrieve the loop via function ActivateXLoop, where X is the name 
##  of the class.

#############################################################################
##  
#F  ActivateMoufangLoop( pos ) 
##    
##  Auxiliary function for activating Moufang loops from the database.
##  <pos> is the position of the loop in the database.

ActivateMoufangLoop := function( pos )

    local d, parent_pos, parent, S, a, h, e, f, G;

    d := moufang_data[ 3 ][ pos ]; #data

    if d[ 1 ] > 0 then # must activate parent first
        #determine position of parent
        parent_pos := pos - 1;
        while moufang_data[ 3 ][ parent_pos ][ 1 ] > 0 do 
            parent_pos := parent_pos - 1;
        od;
        parent_pos := parent_pos + d[ 1 ] - 1;
        parent := ActivateMoufangLoop( parent_pos  );

        if d[ 2 ] = "C" then #cyclic modification
            S := List( d[ 3 ], i -> Elements( parent )[ i ] );
            a := Elements( parent )[ d[ 4 ] ];
            h := Elements( parent )[ d[ 5 ] ];
            return LoopByCyclicModification( parent, S, a, h );
        fi;

        if d[ 2 ] = "D" then # dihedral modification
            S := List( d[ 3 ], i -> Elements( parent )[ i ] );
            e := Elements( parent )[ d[ 4 ] ];
            f := Elements( parent )[ d[ 5 ] ];
            h := Elements( parent )[ d[ 6 ] ];
            return LoopByDihedralModification( parent, S, e, f, h );
        fi;
    fi;

    # no parent; 
    if d[ 2 ]="G" then # loop of type MG2
        G := AllGroups( d[ 3 ], IsCommutative, false)[ d[ 4 ] ];  #relies on GAP group libraries !!
        return LoopMG2( G );
    fi;
    if d[ 2 ]="T" then # special loop (direct product of an old loop and a cyclic group)
        parent := MoufangLoop( d[ 3 ][ 1 ], d[ 3 ][ 2 ]);
        return DirectProduct( parent, CyclicGroup( d[ 3 ][ 3 ] ) );    
    fi;

end;

#############################################################################
##  
#F  ActivateSteinerLoop( pos, n ) 
##    
##  Auxiliary function activating Steiner loops from the database.
##  <pos> is the position of the loop in the database.
##  <n> is the order of the loop.
## 
##  The database steiner_data contains blocks of steiner triple systems.
##  If the system is on k points, the poitns are labelled 0,...,k-1.
##  The constructed Steiner loop has elements labelled 1,...,k+1=n

ActivateSteinerLoop := function( pos, n )

    local d, blocks, i, T, i_in, ij_in, j, MyInt;

    #############################################################################
    ##  
    #F  MyInt( s ) 
    ##    
    ##  Auxiliary function. 
    ##  Given a digit or a lower case letter, returns the numerical value, where
    ##  a = 10, f=15

    MyInt := function( s )
        return Position( "0123456789abcdef", s ) - 1;
    end;
    
    d := steiner_data[ 3 ][ pos ]; # data for the loop = three strings 
    #creating the blocks
    blocks := []; 
    for i in [1..Length( d[ 1 ] )] do
        Add( blocks, [ MyInt( d[1][i] ), MyInt( d[2][i] ), MyInt( d[3][i] ) ] );
    od;
    
    #creating the multiplication table
    T := List( [1..n], i->[1..n] );
    for i in [1..n] do T[i][1] := i; od;
    for i in [0..n-2] do 
        i_in := Filtered( blocks, B->i in B);
        for j in [0..n-2] do
            if j=i then T[i+2][j+2] := 1; fi;
            if not j=i then
                ij_in := Filtered( i_in, B->j in B )[1]; #unique block;
                T[i+2][j+2] := Difference( ij_in, [i,j])[1] + 2;
            fi;
        od;
    od;
    
    return LoopByCayleyTable( T );
end;

#############################################################################
##  
#F  ActivateCCLoop( n, m ) 
##    
##  Activates a CC-loop from the library.
##  See manual for complete discussion concerning this library.

ActivateCCLoop := function( n, m )

    local T, x, y, k, a, b, p, SmallestNonsquare;

    #############################################################################
    ##  
    #F  SmallestNonsquare( p ) 
    ##    
    ##  Auxiliary function. 
    ##  Returns the smallest nonsquare modulo p.

    SmallestNonsquare := function( p )
        local squares, i;
        squares := Set( [1..p-1], i->i^2 mod p );
        for i in [1..p-1] do 
            if not i in squares then return i; fi;
        od;
        return fail; #will never happen
    end;

    # parameters n, m are already checked to be permissible
    if (n mod 2) = 0 then # of the form 2*p
        p := n/2;
        T := List( [1..n], i -> [1..n ] );
        for a in [0..p-1] do for b in [0..p-1] do
            T[a+1][b+1]:= ((a+b) mod p) + 1;
            T[a+1][p+b+1] := p + ((-a+b) mod p ) + 1;
            T[p+a+1][b+1] := p + ((a+b) mod p) + 1;
            T[p+a+1][p+b+1] := ((1-a+b) mod p) + 1;
        od; od;
        return LoopByCayleyTable( T );
    fi;

    # of the form p^2
    p := Sqrt( n );
    T := List([1..n], i->[1..n]);
    if m = 1 then
        for x in [0..n-1] do for y in [0..n-1] do
            T[ x+1 ][ y+1 ] := ((x + y + p*(x^2)*y) mod n) + 1;
        od; od;
    elif m = 2 then
        k := SmallestNonsquare( p );
        for x in [0..n-1] do for y in [0..n-1] do
            T[ x+1 ][ y+1 ] := ((x + y + k*p*(x^2)*y) mod n) + 1;
        od; od;
    elif m = 3 then
        for x in [0..p-1] do for y in [0..p-1] do for a in [0..p-1] do for b in [0..p-1] do
            T[ x*p+a+1 ][ y*p+b+1 ] := ((x+y) mod p)*p + ((a+b+(x^2)*y) mod p) + 1;
        od; od; od; od; 
    fi;
    return LoopByCayleyTable( T );
end;        

#############################################################################
##  READING LOOPS FROM THE LIBRARY - GENERIC METHOD
##  -------------------------------------------------------------------------

#############################################################################
##  
#F  LibraryLoop( name, n, m ) 
##  
##  returns the <m>th loop of order <n> from the library named <name>

InstallGlobalFunction( LibraryLoop, function( name, n, m )

    local lib, implemented_orders, NOL, loop, pos_n, p, PG, m_inv, PosInDB, root, half;

    lib := LibraryByName( name );

    # extent of the library
    implemented_orders := lib[ 1 ];
    
    # number of loops of given order in the library
    NOL := lib[ 2 ];
    
    # testing arguments
    if (not n in Integers) or (not m in Integers) or not (n>0) or not (m>0) then
        Error("Both arguments must be positive integers.");
    fi;
    if not name = "CC" then
        if not n in implemented_orders then Error("Order ", n, " not implemented."); fi;    
        pos_n := Position( implemented_orders, n );
        if NOL[ pos_n ] < m then 
            if NOL[ pos_n ] = 1 then
                Error("There is only ", NOL[ pos_n ], " ", name, " loop of order ", n, " in the library."); 
            else
                Error("There are only ", NOL[ pos_n ], " ", name, " loops of order ", n, " in the library."); 
            fi;
        fi;    
    else # CC-loops
        root := Sqrt( n );
        half := n/2;
        if ( not ( IsInt( root ) and IsOddInt( root) and IsPrime( root ) ) ) 
            and ( not ( IsInt( half ) and IsOddInt( half ) and IsPrime( half ) ) ) then
            Error("There are only CC-loops of size p^2 and 2*p for odd primes p in the library.");
        fi;
        if IsEvenInt( n ) and ( not m=1 ) then # 2*p
           Error("There is only 1 nonassociative CC-loop of size 2*p for an odd prime p.");
        fi;
        if (not IsEvenInt( n )) and (not m in [1..3]) then #p^2
            Error("There are only 3 nonassociative CC-loops of size p^2 for an odd prime p.");
        fi;
    fi;                                            
    
    #############################################################################
    ##  
    #F  PosInDB( m ) 
    ##  
    ##  Auxiliary function. Returns the position of the <m>th loop of order <n>
    ##  in the datanase. Note that <n> is assigned specified globally.
    PosInDB := function( m )
        local p;
        for p in [1 .. pos_n - 1] do
            m := m + NOL[ p ];
        od;
        return m;
    end;
    
    # activating the desired loop (treat cases separately below)
    if name = "left Bol" then 
        loop := LoopByCayleyTable( lib[ 3 ][ m ] );
        SetIsLeftBolLoop( loop, true );
    elif name = "Moufang" then
        # renaming loops so that they agree with Goodaire's classification
        PG := List([1..64], i->());
        PG[16] :=   (2,5,3,4);
        PG[24] :=   (1,3,4)(2,5);
        PG[32] :=   (1,4,61,7,64,31,19,44,3,51,24,46,22,49,65,9,50,39,41,2,63,27,57,14,5,55,13,69,32,58,17,53,15)
            (6,71,28,36,30,47,18,42,26,60,21,59,35,52,25,40,23,43,29,56,20,66,8,70,33,67,38,54,16,62,37,11,68,34,45,10,48,12);
        PG[36] :=   (1,2);
        PG[40] :=   (1,2,5,3,4);
        PG[48] :=   (1,11,43,49,30,7,28,23,36,21,50,31,41,32,46,51,29,12,42,15)
            (2,25,24,35,22,8,39,5,6,38,16,19,17,37,34,33,4,47,13,20,10,40,48,14,3,26,45)
            (9,27,44);
        PG[56] :=   (1,2,4);
        PG[60] :=   (1,2);
        m_inv := m^Inverse( PG[ n ] );
        # activating the loop
        loop := ActivateMoufangLoop( PosInDB( m_inv ) );
        SetIsMoufangLoop( loop, true );
    elif name = "Paige" then
        loop := LoopByCayleyTable( lib[ 3 ][ 1 ] ); #only one loop there at this point
        SetIsMoufangLoop( loop, true );
    elif name = "Steiner" then
        loop := ActivateSteinerLoop( PosInDB( m ), n );
        SetIsSteinerLoop( loop, true );
    elif name = "CC" then
        loop := ActivateCCLoop( n, m );
        SetIsCCLoop( loop, true );
    elif name = "interesting" then
        loop := LoopByCayleyTable( lib[ 3 ][ PosInDB( m ) ][ 1 ] );
        SetName( loop, lib[ 3 ][ PosInDB( m ) ][ 2 ] );
    fi;
    
    # setting the name
    if not name = "interesting" then
        SetName( loop, Concatenation( "<", name, " loop ", String( n ), "/", String( m ), ">" ) );
    fi;
    
    # returning the loop
    return loop;
end);

#############################################################################
##  SMALL BOL LOOPS
##  -------------------------------------------------------------------------

#############################################################################
##  
#F  LeftBolLoop( n, m ) 
##    
##  mth proper left Bol loop of order n

InstallGlobalFunction( LeftBolLoop, function( n, m )
    return LibraryLoop( "left Bol", n, m );
end);

#############################################################################
##  SMALL MOUFANG LOOPS
##  ------------------------------------------------------------------------

# This will be used later to check if moufang_discriminators 
# have been activated
moufang_discriminators := [];
moufang_loops_by_discriminators := [];

#############################################################################
##  
#F  MoufangLoop( n, m ) 
##    
##  mth proper Moufang loop of order n

InstallGlobalFunction( MoufangLoop, function( n, m )
    return LibraryLoop( "Moufang", n, m );
end);

#############################################################################
##  SMALL PAIGE LOOPS
##  ------------------------------------------------------------------------

#############################################################################
##  
#F  PaigeLoop( q ) 
##    
##  Paige loop constructed over GF( q )

InstallGlobalFunction( PaigeLoop, function( q )
    if not q=2 then return Error( "Only q=2 is implemented."); fi;
    return LibraryLoop( "Paige", 120, 1 );
end);

#############################################################################
##  SMALL STEINER LOOPS
##  ------------------------------------------------------------------------

#############################################################################
##  
#F  SteinerLoop( n, m ) 
##    
##  mth Steiner loop of order n

InstallGlobalFunction( SteinerLoop, function( n, m )
    return LibraryLoop( "Steiner", n, m );
end);

#############################################################################
##  CC-LOOPS OF ORDER p^2 AND 2*p FOR ODD PRIME p
##  ------------------------------------------------------------------------

#############################################################################
##  
#F  CCLoop( n, m ) 
##    
##  mth CC-loop of order n

InstallGlobalFunction( CCLoop, function( n, m )
    return LibraryLoop( "CC", n, m );
end);

#############################################################################
##  INTERESTING LOOPS
##  ------------------------------------------------------------------------

#############################################################################
##  
#F  InterestingLoop( n, m ) 
##    
##  <m>th interesting loop of order <n>

InstallGlobalFunction( InterestingLoop, function( n, m )
    return LibraryLoop( "interesting", n, m );
end);
