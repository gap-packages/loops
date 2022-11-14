#############################################################################
##
#W  examples.gi              Examples [loops]
##  
##  
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  Binding global variable LOOPS_aux 
##
##  The variable is used for temporary storage throughout the package. 
##  We therefore do not want the variable to be read only.

LOOPS_aux := [];

#############################################################################
##  READING DATA
##  -------------------------------------------------------------------------

# up to isomorphism
ReadPackage("loops", "data/leftbol.tbl");       # left Bol loops
ReadPackage("loops", "data/moufang.tbl");       # Moufang loops
ReadPackage("loops", "data/paige.tbl");         # Paige loops
ReadPackage("loops", "data/code.tbl");          # code loops
ReadPackage("loops", "data/steiner.tbl");       # Steiner loops
ReadPackage("loops", "data/cc.tbl");            # CC loops
ReadPackage("loops", "data/rcc.tbl");           # RCC loops (more is read upon calling RCCLoop(n,m) for the first time
ReadPackage("loops", "data/small.tbl");         # small loops
ReadPackage("loops", "data/interesting.tbl");   # interesting loops
ReadPackage("loops", "data/nilpotent.tbl");     # nilpotent loops
ReadPackage("loops", "data/automorphic.tbl");   # automorphic loops
ReadPackage("loops", "data/rightbruck.tbl");    # right Bruck loops

# up to isotopism
ReadPackage("loops", "data/itp_small.tbl");     # small loops up to isotopism

#############################################################################
##  DISPLAYING INFORMATION ABOUT A LIBRARY
##  -------------------------------------------------------------------------

#############################################################################
##  
#F  LOOPS_LibraryByName( name ) 
##    
##  Auxiliary. Returns the library corresponding to <name>.

InstallGlobalFunction( LOOPS_LibraryByName, 
function( name )
    #up to isomorphism
    if name = "left Bol" then return LOOPS_left_bol_data; 
    elif name = "Moufang" then return LOOPS_moufang_data;
    elif name = "Paige" then return LOOPS_paige_data;
    elif name = "code" then return LOOPS_code_data;
    elif name = "Steiner" then return LOOPS_steiner_data;
    elif name = "CC" then return LOOPS_cc_data;
    elif name = "RCC" then return LOOPS_rcc_data;
    elif name = "small" then return LOOPS_small_data;
    elif name = "interesting" then return LOOPS_interesting_data;
    elif name = "nilpotent" then return LOOPS_nilpotent_data;
    elif name = "automorphic" then return LOOPS_automorphic_data;
    elif name = "right Bruck" then return LOOPS_right_bruck_data;
    #up to isotopism
    elif name = "itp small" then return LOOPS_itp_small_data;
    fi;
end);

#############################################################################
##  
#F  DisplayLibraryInfo( name ) 
##    
##  Display information about library named <name>.

InstallGlobalFunction( DisplayLibraryInfo, function( name )
    local s, lib, k;
    # up to isomorphism
    if name = "left Bol" or name = "right Bol" then
        s := Concatenation( "The library contains all nonassociative ", name, " loops of order less than 17\nand all nonassociative ", name, " loops of order p*q, where p>q>2 are primes." );
    elif name = "Moufang" then
        s := "The library contains all nonassociative Moufang loops \nof order less than 65, and all nonassociative Moufang \nloops of order 81 and 243.";
    elif name = "Paige" then
        s := "The library contains the smallest nonassociative finite \nsimple Moufang loop.";
    elif name = "code" then
        s := "The library contains all nonassociative even code loops \nof order less than 65.";
    elif name = "Steiner" then
        s := "The library contains all nonassociative Steiner loops \nof order less or equal to 16. It also contains the \nassociative Steiner loops of order 4 and 8.";
    elif name = "CC" then
        s := "The library contains all CC loops of order\n2<=2^k<=64, 3<=3^k<=81, 5<=5^k<=125, 7<=7^k<=343,\nall nonassociative CC loops of order less than 28,\nand all nonassociative CC loops of order p^2 and 2*p for any odd prime p.";
    elif name = "RCC" or name = "LCC" then
        s := Concatenation( "The library contains all nonassociative ", name, " loops of order less than 28." );
    elif name = "small" then
        s := "The library contains all nonassociative loops of order less than 7.";
    elif name = "interesting" then
        s := "The library contains a few interesting loops.";
    elif name = "nilpotent" then
        s := "The library contains all nonassociative nilpotent loops \nof order less than 12.";
    elif name = "automorphic" then
        s := "The library contains:\n";
        s := Concatenation(s," - all nonassociative automorphic loops of order less than 16,\n");
        s := Concatenation(s," - all commutative automorphic loops of order 3, 9, 27, 81.");
    elif name = "left Bruck" or name = "right Bruck" then
        s := Concatenation( "The library contains all ", name, " loops of orders 3, 9, 27 and 81." );
    # up to isotopism
    elif name = "itp small" then
        s := "The library contains all nonassociative loops of order less than 7 up to isotopism.";
    else
        Info( InfoWarning, 1, Concatenation(
            "The admissible names for loop libraries are: \n",
            "\"automorphic\", \"CC\", \"code\",  \"interesting\", \"itp small\", \"LCC\", \"left Bol\",  \"left Bruck\", \"Moufang\", \"nilpotent\", \"Paige\", \"right Bol\", \"right Bruck\", \"RCC\", \"small\", \"Steiner\"."
        ) );
        return fail;
    fi;
    
    s := Concatenation( s, "\n------\nExtent of the library:" );
    
    # renaming for data access
    if name = "right Bol" then name := "left Bol"; fi;
    if name = "LCC" then name := "RCC"; fi;
    if name = "left Bruck" then name := "right Bruck"; fi;
    
    lib := LOOPS_LibraryByName( name );
    for k in [1..Length( lib[ 1 ] ) ] do
        if lib[ 2 ][ k ] = 1 then
            s := Concatenation( s, "\n   ", String( lib[ 2 ][ k ] ), " loop of order ", String( lib[ 1 ][ k ] ) );
        else
            s := Concatenation( s, "\n   ", String( lib[ 2 ][ k ] ), " loops of order ", String( lib[ 1 ][ k ] ) );
        fi;
    od;
    if name = "left Bol" then
        s := Concatenation( s, "\n   (p-q)/2 loops of order p*q for primes p>q>2 such that q divides p-1");
        s := Concatenation( s, "\n   (p-q+2)/2 loops of order p*q for primes p>q>2 such that q divides p+1" );
    fi;
    if name = "CC" then
        s := Concatenation( s, "\n   3 loops of order p^2 for every prime p>7,\n   1 loop of order 2*p for every odd prime p" );
    fi;
    s := Concatenation( s, "\n" );
    Print( s );
    return true;
end);

#############################################################################
##  AUXILIARY FUNCTIONS
##  -------------------------------------------------------------------------
##  When the data in the database is encoded in some way, we usually
##  retrieve the loop via function LOOPS_ActivateXLoop, where X is the name 
##  of the class.

#############################################################################
##  
#F  LOOPS_SmallestNonsquare( p ) 
##    
##  Auxiliary function. 
##  Returns the smallest nonsquare modulo p.

InstallGlobalFunction( LOOPS_SmallestNonsquare,
function( p )
    local squares, i;
    squares := Set( [1..p-1], i->i^2 mod p );
    for i in [2..p-1] do 
        if not i in squares then return i; fi;
    od;
    return fail; # will never happen if p>2 is prime
end);

#############################################################################
##  
#F  LOOPS_ActivateLeftBolLoopPQ( p, q, m ) 
##    
##  Auxiliary function for activating left Bol loop of order p*q.
##  See paper by Kinyon, Nagy and Vojtechovsky.
##  p>q>2 are primes such that q divides p^2-1, m is an integer in the range [1..(p-q)/2] or [1..(p-q+2)/2]

InstallGlobalFunction( LOOPS_ActivateLeftBolLoopPQ,
function( p, q, m )
    local F, omega, lambda, ev, inv_ev, params, t, sqrt_t, final_params, alpha, theta, GFp, ct, i, j, k, l, u, v, w, x, y;
    F := GF(p^2);
    omega := PrimitiveRoot( F )^((p^2-1)/q);
    lambda := omega + omega^(-1);
    ev := List([0..q-1], j -> omega^j);
    inv_ev := List([0..q-1], j -> omega^(-j));
    if IsInt((p-1)/q) then
        params := List([2..p-1], j->j*One(F)); # GF(p); 0 and 1 correspond to isomorphic nonabelian groups
        params := Filtered( params, x -> not ((One(F) - x^(-1)) in ev) );
    else # q divides p+1
        t := LOOPS_SmallestNonsquare( p );
        sqrt_t := RootsOfPolynomial( F, X(F,"x")^2 - t )[ 1 ]; # a squre root of t in GF(p^2) 
        params := List([0..p-1], j -> (1/2)*One(F) + j*One(F)*sqrt_t );  # 1/2 + GF(p)\sqrt{t} 
        params := Filtered( params, x -> not ((One(F) - x^(-1)) in ev) );
    fi;
    final_params := [];
    for x in params do
        if not ( (One(F)-x) in final_params ) then
            Add( final_params, x );
        fi;
    od;
    alpha := final_params[ m ]; 
    theta := alpha*ev + (One(F)-alpha)*inv_ev; 
    theta := List( theta, x -> x^(-1) );
    GFp := List([0..p-1], j -> j*One(F));
    theta := List( theta, x -> Position( GFp, x )-1 );
    # construct the Cayley table according to (a^i b^j)*(a^k b^l) = a^{i+k} b^{ w + (l+w)*theta[k]^{-1}*theta[i+k], where w+w*theta[i] = j    
    ct := List([1..p*q], i -> 0*[1..p*q]);
    for i in [0..q-1] do for j in [0..p-1] do for k in [0..q-1] do for l in [0..p-1] do
        u := (i+k) mod q;
        w := ( j/(1+theta[i+1]) ) mod p;
        v := ( w + (l+w)*theta[ ((i+k) mod q) + 1 ]/theta[k+1] ) mod p;
        x := i*p+j+1;
        y := k*p+l+1;
        ct[x][y] := u*p+v+1;
    od; od; od; od; 
    # return the loop
    return LoopByCayleyTable( ct ); 
end);

#############################################################################
##  
#F  LOOPS_ActivateLeftBolLoop( pos_n, m, case ) 
##    
##  Auxiliary function for activating left Bol loops from the database.
##  case = [p,q] if it is a left Bol loop of order p*q, with p>q>2 primes such that q divides p^2-1
##  case = false otherwise
##  pos_n is meaningless when case = [p,q]

InstallGlobalFunction( LOOPS_ActivateLeftBolLoop,
function( pos_n, m, case )
    local rep_m, ct, perm;
    if not case=false then # p*q
        return LOOPS_ActivateLeftBolLoopPQ( case[1], case[2], m );
    fi;    
    # in database 
    rep_m := m;
    # searching for a Cayley table on which the loop is based
    while not IsString( LOOPS_left_bol_data[ 3 ][ pos_n ][ rep_m ] ) do 
        rep_m := rep_m - 1;
    od;
    if rep_m = m then # loop given by encoded Cayley table
        ct := LOOPS_DecodeCayleyTable( LOOPS_left_bol_data[ 3 ][ pos_n ][ m ] );
    else # loop given as an isotope of another loop
        ct := LOOPS_DecodeCayleyTable( LOOPS_left_bol_data[ 3 ][ pos_n ][ rep_m ] );
        perm := PermList( ct[ LOOPS_left_bol_data[ 3 ][ pos_n ][ m ] ] );
        ct := Set( List( ct, row -> OnTuples( row, perm^-1 ) ) );
    fi;
    return LoopByCayleyTable( ct );
    
end);

#############################################################################
##  
#F  LOOPS_ActivateMoufangLoop( n, pos_n, m ) 
##    
##  Auxiliary function for activating Moufang loops from the database.

InstallGlobalFunction( LOOPS_ActivateMoufangLoop,
function( n, pos_n, m )
    local d, parent_m, parent, S, a, h, e, f, G, ret, x, row, y, b, c, z;
    
    d := LOOPS_moufang_data[ 3 ][ pos_n ][ m ]; #data
    
    # orders 81 and 243 are represented as central extensions
    if n = 81 or n = 243 then 
        return LoopByExtension( 
            IntoLoop( SmallGroup( d[1], d[2] ) ),                       # K
            IntoLoop( SmallGroup( d[3], d[4] ) ),                       # F
            List([1..d[3]], i -> () ),                                  # trivial action F -> Aut( K )
            LOOPS_DecodeCocycle( [ n/d[1], false, d[5] ], [1..d[1]] )   # cocycle
        );
    fi;
    
    # all other orders
    if d[ 1 ] > 0 then # must activate parent first
        # determine position of parent ( d[1] gives it relative to the class leader )
        while LOOPS_moufang_data[ 3 ][ pos_n ][ m ][ 1 ] > 0 do
            m := m - 1;
        od;
        m := m + d[ 1 ] - 1;
        parent := LOOPS_ActivateMoufangLoop( n, pos_n, m );

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
        G := AllSmallGroups( d[ 3 ], IsCommutative, false)[ d[ 4 ] ];  #relies on GAP group libraries !!
        return LoopMG2( G );
    fi;
    if d[ 2 ]="T" then # special loop (direct product of an old loop and a cyclic group)
        parent := MoufangLoop( d[ 3 ][ 1 ], d[ 3 ][ 2 ]);
        return DirectProduct( parent, CyclicGroup( d[ 3 ][ 3 ] ) );    
    fi;

end);

#############################################################################
##  
#F  LOOPS_ActivateSteinerLoop( n, pos_n, m ) 
##    
##  Auxiliary function activating Steiner loops from the database.
##
##  The database LOOPS_steiner_data contains blocks of steiner triple systems.
##  If the system is on k points, the points are labelled 0,...,k-1.
##  The constructed Steiner loop has elements labelled 1,...,k+1=n

InstallGlobalFunction( LOOPS_ActivateSteinerLoop,
function( n, pos_n, m )
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
    
    d := LOOPS_steiner_data[ 3 ][ pos_n ][ m ]; # data for the loop = three strings 
    # creating the blocks
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
end);

#############################################################################
##  
#F  LOOPS_ActivateRCCLoop( n, pos_n, m ) 
##    
##  Activates an RCC loop from the library.
##  See manual for complete discussion concerning this library.

InstallGlobalFunction( LOOPS_ActivateRCCLoop,
function( n, pos_n, m )
    local pos_m, g, nr_conj_classes, data, data2, next_compactified, x, i, rel_m, G, section, pos_conjugacy_classes;

    if  IsEmpty( LOOPS_rcc_transitive_groups ) then # data not read yet
        ReadPackage( "loops", "data/rcc/rcc_transitive_groups.tbl" );
    fi;
    # determining the transitive group corresponding to pos_n, m
    pos_m := Length( LOOPS_rcc_data[ 3 ][ pos_n ][ 1 ] ); # nr of transitive groups associated with order n
    while LOOPS_rcc_data[ 3 ][ pos_n ][ 2 ][ pos_m ] > m do 
        pos_m := pos_m - 1;
    od;
    g := LOOPS_rcc_data[ 3 ][ pos_n ][ 1 ][ pos_m ]; # index of transitive group (of degree n) in GAP library
    # activating data for the group, if needed
    if not IsBound( LOOPS_rcc_sections[ pos_n ][ pos_m ] ) then
        # data must be read from file and decoded
        ReadPackage( "loops", Concatenation( "data/rcc/sections", String(n), ".", String(g), ".tbl" ) );
        # variable LOOPS_aux is now read and ready to be processed
        nr_conj_classes := Length( LOOPS_rcc_transitive_groups[ pos_n ][ pos_m ][ 2 ] );
        data := SplitString( LOOPS_aux, " " );
        data2 := [];
        next_compactified := false;
        for x in data do 
            if x="" then # the next entry is compactified, eg. "a$X" means "a", "$", "X"
                next_compactified := true;
            elif not next_compactified then
                Add( data2, x );
            else # compactified string
                next_compactified := false;
                for i in [1..Length(x)] do
                    Add( data2, x{[i..i]} );    # Add( data2, x[i] ) is not safe when data2 is empty
                od;
            fi;
        od;
        data := List( data2, x -> LOOPS_ConvertToDecimal( x, 91 ) ); 
        # reconstructing the sequence from the difference sequence
        for i in [2..Length( data )] do
            data[ i ] := data[ i-1 ] - data[ i ];
        od;
        LOOPS_rcc_sections[ pos_n ][ pos_m ] := data; 
    fi;
    # data is now loaded
    G := TransitiveGroup(n, g);
    rel_m := m - LOOPS_rcc_data[ 3 ][ pos_n ][ 2 ][ pos_m ] + 1;  # relative position of the loop in the file for G
    section := [];
    nr_conj_classes := Length( LOOPS_rcc_transitive_groups[ pos_n ][ pos_m ][ 2 ] );
    if not LOOPS_rcc_conjugacy_classes[ 1 ] = [ n, g ] then # must calculate conjugacy classes, so let's reset old data
        LOOPS_rcc_conjugacy_classes[ 1 ] := [ n, g ];
		LOOPS_rcc_conjugacy_classes[ 2 ] := List( [1..nr_conj_classes], x->[] );
    fi;
	x := LOOPS_rcc_sections[ pos_n ][ pos_m ][ rel_m ];
	x := LOOPS_ConvertFromDecimal( x, 2, nr_conj_classes ); # convert to a binary string of prescribed length
	pos_conjugacy_classes := Positions( x, '1' );
    for i in [1..nr_conj_classes] do
        if LOOPS_rcc_conjugacy_classes[ 2 ][ i ] = [] then
            LOOPS_rcc_conjugacy_classes[ 2 ][ i ] := Elements( ConjugacyClass( G, LOOPS_rcc_transitive_groups[ pos_n ][ pos_m ][ 2 ][ i ] ) );
        fi;
    od;
	section := Concatenation( LOOPS_rcc_conjugacy_classes[ 2 ]{pos_conjugacy_classes} );
    Add( section, One( G ) ); # the trivial class is contained in all loops and never stored
    Sort( section );
    return LoopByRightSection( section );
end);

#############################################################################
##  
#F  LOOPS_ActivateCCLoop( n, pos_n, m, case ) 
##    
##  Activates a CC-loop from the library.
##  The argument p_case is set to [p,"p^2"] if n = p^2, to [p,"2*p"] if n=2*p, and false otherwise.
##  See manual for complete discussion concerning this library.

InstallGlobalFunction( LOOPS_ActivateCCLoop,
function( n, pos_n, m, case )
    local powers, p, i, k, F, basis, coords, coc, T, a, b, x, y;
    powers := [ ,[4,8,16,32,64],[9,27,81],,[25,125],,[49,343]];
    if n in Union( powers ) then # use cocycles
        # determine p and position of n in database
        p := Filtered([2,3,5,7], x -> n in powers[x])[1];
        pos_n := Position( powers[p], n );
        if not IsBound( LOOPS_cc_cocycles[p] ) then
            # data not read yet, activate once
            ReadPackage( "loops", Concatenation( "data/cc/cc_cocycles_", String(p), ".tbl" ) );
            # decode cocycles and separate coordinates from a long string
            for i in [1..Length(powers[p])] do
                LOOPS_cc_cocycles[ p ][ i ] := List( LOOPS_cc_cocycles[ p ][ i ],
                    c -> LOOPS_DecodeCocycle( [ p^i, c[1], c[2] ], [0..p-1] )
                );
                LOOPS_cc_coordinates[ p ][ i ] := List( LOOPS_cc_coordinates[ p ][ i ],
                    c -> SplitString( c, " " )
                );
            od;
        fi;
        # data is now read
        # determine position of loop in the database
        k := 1;
        while m > Length( LOOPS_cc_coordinates[ p ][ pos_n ][ k ] ) do
            m := m - Length( LOOPS_cc_coordinates[ p ][ pos_n ][ k ] );
            k := k + 1;
        od;
        # factor loop
        F := CCLoop( n/p, LOOPS_cc_used_factors[ p ][ pos_n ][ k ] );
        # basis
        basis := List( LOOPS_cc_bases[ p ][ pos_n ][ k ],
            i -> LOOPS_cc_cocycles[ p ][ pos_n ][ i ]
        );
        # coordinates 
        coords := LOOPS_cc_coordinates[ p ][ pos_n ][ k ][ m ];
        coords := LOOPS_ConvertBase( coords, 91, p, Length( basis ) );
        coords := List( coords, LOOPS_CharToDigit );
        # cocycle
        coc := (coords*basis) mod p;
        coc := List( coc, i -> i+1 ); 
        # return extension of Z_p by F using cocycle and trivial action
        return LoopByExtension( CCLoop(p,1), F, List([1..n/p], i -> () ), coc );
    fi;
    
    if case=false then # use library of RCC loops, must recalculate pos_n
        return LOOPS_ActivateRCCLoop( n, Position(LOOPS_rcc_data[ 1 ], n), LOOPS_cc_data[ 3 ][ pos_n ][ m ] ); 
    fi;

    # parameters n, m are already checked to be permissible
    p := case[ 1 ];
    if case[ 2 ] = "2*p" then # 2*p
        T := List( [1..n], i -> [1..n ] );
        for a in [0..p-1] do for b in [0..p-1] do
            T[a+1][b+1]:= ((a+b) mod p) + 1;
            T[a+1][p+b+1] := p + ((-a+b) mod p ) + 1;
            T[p+a+1][b+1] := p + ((a+b) mod p) + 1;
            T[p+a+1][p+b+1] := ((1-a+b) mod p) + 1;
        od; od;
        return LoopByCayleyTable( T );
    fi;

    # p^2
    T := List([1..n], i->[1..n]);
    if m = 1 then
        for x in [0..n-1] do for y in [0..n-1] do
            T[ x+1 ][ y+1 ] := ((x + y + p*(x^2)*y) mod n) + 1;
        od; od;
    elif m = 2 then
        k := LOOPS_SmallestNonsquare( p );
        for x in [0..n-1] do for y in [0..n-1] do
            T[ x+1 ][ y+1 ] := ((x + y + k*p*(x^2)*y) mod n) + 1;
        od; od;
    elif m = 3 then
        for x in [0..p-1] do for y in [0..p-1] do for a in [0..p-1] do for b in [0..p-1] do
            T[ x*p+a+1 ][ y*p+b+1 ] := ((x+y) mod p)*p + ((a+b+(x^2)*y) mod p) + 1;
        od; od; od; od; 
    fi;
    return LoopByCayleyTable( T );
end);

#############################################################################
##  
#F  LOOPS_ActivateNilpotentLoop( data ) 
##    
##  Activates the nilpotent loop based on data = [ K, F, t ], where
##  K determines a central (normal) subloop,
##  F determines the factor loop, 
##  t determines the cocycle.
##  Understanding K and F:
##      If the value of K or F is in [2,3,4,5], it is the cyclic group of order V.
##      If the value of K or F is 0, it is the Klein group.
##  Understanding t:
##      The cocycle is mapping from F x F to K. Let f = |F|. Let k = |K|.
##      The value t corresponds to a (f-1)x(f-1) array of values in [0..k-1].
##      It is represented by a single integer in base k, with the least 
##      significant digit in the first row and first column, then following
##      the rows.
##      Once t is decoded into a (f-1)x(f-1) array, 1 is added to all entries.
##      Then a first row and forst column of all ones is added to t,
##      resulting in a f x f array.
##  The loop is then obtained via LoopByExtension( K, F, phi, t), where
##  phi is trivial.

InstallGlobalFunction( LOOPS_ActivateNilpotentLoop,
function( data )
    local K, F, f, k, t, theta, i, j, phi;
    
    # preparing normal subloop and factor loop
    if data[ 1 ] = 0 then 
        K := IntoLoop( Group( (1,2),(3,4) ) ); 
    else
        K := IntoLoop( CyclicGroup( data[ 1 ] ) );
    fi;
    if data[ 2 ] = 0 then 
        F := IntoLoop( Group( (1,2),(3,4) ) );
    else
        F := IntoLoop( CyclicGroup( data[ 2 ] ) );    
    fi;
    
    # preparing cocycle
    f := Size( F );
    k := Size( K );
    t := data[ 3 ];
    theta := List( [1..f], i->[1..f] );
    for i in [2..f] do
        theta[ 1 ][ i ] := 1;
    od;
    for i in [2..f] do for j in [2..f] do
        theta[ i ][ j ] := t mod k;
        t := (t - theta[i][j])/k;
        theta[ i ][ j ] := theta[ i ][ j ] + 1;
    od; od;
    
    # preparing trivial action
    phi := List([1..f], i -> () );
    
    # constructing the loop
    return LoopByExtension( K, F, phi, theta );

end);

#############################################################################
##  
#F  LOOPS_ActivateAutomorphicLoop( n, m ) 
##    
##  Activates an automorphic loop from the library.

InstallGlobalFunction( LOOPS_ActivateAutomorphicLoop,
function( n, m )
    # returns the associated Gamma loop (which here always happens to be automorphic)
    # improve later
    local P, L, s, Ls, ct, i, j, pos, f;
    P := LeftBruckLoop( n, m );
    L := LeftMultiplicationGroup( P );;
    s := List(Elements(L), x -> x^2 );;
    Ls := List([1..n], i -> LeftTranslation( P, Elements(P)[i] ) );;
    ct := List([1..n],i->0*[1..n]);;
    for i in [1..n] do for j in [1..n] do
	   pos := Position( s, Ls[i]*Ls[j]*Ls[i]^(-1)*Ls[j]^(-1) );
	   f := Elements(L)[pos];
	   ct[i][j] := 1^(f*Ls[j]*Ls[i]);
    od; od;
    return LoopByCayleyTable(ct);
end);

#############################################################################
##  
#F  LOOPS_ActivateRightBruckLoop( n, m ) 
##    
##  Activates a right Bruck loop from the library.

InstallGlobalFunction( LOOPS_ActivateRightBruckLoop,
function( n, m )
    local pos_n, factor_id, F, basis, coords, coc;
    # factor loop
    pos_n := Position( [27,81], n );
    factor_id := LOOPS_CharToDigit( LOOPS_right_bruck_coordinates[ pos_n ][ m ][ 1 ] );
    F := RightBruckLoop( n/3, factor_id );
    # basis (only decode cocycles at first usage)
    if IsString( LOOPS_right_bruck_cocycles[ pos_n ][ 1 ][ 3 ] ) then # not converted yet
        LOOPS_right_bruck_cocycles[ pos_n ] := List( LOOPS_right_bruck_cocycles[ pos_n ],
            coc -> LOOPS_DecodeCocycle( coc, [0,1,2] )
        );
    fi;
    basis := LOOPS_right_bruck_cocycles[ pos_n ];
    # coordinates determining the cocycle
    coords := LOOPS_right_bruck_coordinates[ pos_n ][ m ];
    coords := coords{[2..Length(coords)]}; # remove the character that determines factor id
    coords := LOOPS_ConvertBase( coords, 91, 3, Length( basis ) );
    coords := List( coords, LOOPS_CharToDigit );
    # calculate cocycle
    coc := (coords*basis) mod 3;
    coc := coc + 1;
    # return extension of Z_3 by F using cocycle and trivial action
    return LoopByExtension( RightBruckLoop(3,1), F, List([1..n/3], i -> () ), coc );
end);  

#############################################################################
##  READING LOOPS FROM THE LIBRARY - GENERIC METHOD
##  -------------------------------------------------------------------------

#############################################################################
##  
#F  LibraryLoop( name, n, m ) 
##  
##  returns he <m>th loop of order <n> from the library named <name>

InstallGlobalFunction( LibraryLoop, function( name, n, m )

    local lib, implemented_orders, NOL, loop, pos_n, p, q, divs, PG, m_inv, root, half, case, g, h;

    # selecting data library
    lib := LOOPS_LibraryByName( name );

    # extent of the library
    implemented_orders := lib[ 1 ];
    
    # number of loops of given order in the library
    NOL := lib[ 2 ];
    
    # testing arguments
    if (not n in Integers) or (not m in Integers) or not (n>0) or not (m>0) then
        Error("LOOPS: Both arguments must be positive integers.");
    fi;
    # parameters for handling systematic cases, such as CCLoop( p^2, 1 )
    pos_n := fail;
    case := false; 
    if name="left Bol" then
        divs := DivisorsInt( n );
        if Length( divs ) = 4 and not IsInt( divs[3]/divs[2] ) then # case n = p*q
            q := divs[ 2 ];
            p := divs[ 3 ];
            case := [p,q];
            if not (IsOddInt( q ) and IsInt((p^2-1)/q)) then
                Error("LOOPS: Nonassociative ", name, " loops of order p*q exist only for primes p>q>2 such that q divides p^2-1.");
            fi;
            if IsInt((p-1)/q) and (not m in [1..(p-q)/2]) then
                Error("LOOPS: There are only ", (p-q)/2, " nonassociative ", name, " loops of order ", n, ".");
            fi;
            if IsInt((p+1)/q) and (not m in [1..(p-q+2)/2]) then
                Error("LOOPS: There are only ", (p-q+2)/2, " nonassociative ", name, " loops of order ", n, ".");
            fi;
        fi;
    fi;
    if name="CC" then 
        divs := DivisorsInt( n );
        if Length( divs ) = 3 and divs[ 2 ] > 7 then # case p^2, p>7
            p := divs[ 2 ];
            case := [p,"p^2"];
            if not m in [1..3] then
                Error("LOOPS: There are only 3 nonassociative CC-loops of order p^2 for an odd prime p.");
            fi;
        elif Length( divs ) = 4 and not IsInt( divs[3]/divs[2] ) and not n=21 then # p*q
            p := divs[ 3 ];
            case := [p,"2*p"];
            if not divs[2] = 2 then
                Error("LOOPS: Order ", n, " not implemented.");
            fi;
            if not m=1 then 
                Error("LOOPS: There is only 1 nonassociative CC-loop of order 2*p for an odd prime p.");
            fi;
        fi;
    fi;
    if case=false then
        if not n in implemented_orders then
            Error("LOOPS: Order ", n, " not implemented.");
        fi;
        pos_n := Position( implemented_orders, n );
        if NOL[ pos_n ] < m then 
            if NOL[ pos_n ] = 1 then
                Error("LOOPS: There is only ", NOL[ pos_n ], " ", name, " loop of order ", n, " in the library."); 
            else
                Error("LOOPS: There are only ", NOL[ pos_n ], " ", name, " loops of order ", n, " in the library."); 
            fi;
        fi; 
    fi;                                     
       
    # activating the desired loop (treat cases separately below)
    
    # up to isomorphism
    if name = "left Bol" then 
        loop := LOOPS_ActivateLeftBolLoop( pos_n, m, case );
        SetIsLeftBolLoop( loop, true );
    elif name = "Moufang" then
        # renaming loops so that they agree with Goodaire's classification
        PG := List([1..243], i->());
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
        loop := LOOPS_ActivateMoufangLoop( n, pos_n, m_inv );
        SetIsMoufangLoop( loop, true );
    elif name = "Paige" then
        loop := LoopByCayleyTable( lib[ 3 ][ 1 ][ 1 ] ); #only one loop there at this point
        SetIsMoufangLoop( loop, true );
    elif name = "code" then
        loop := LibraryLoop( "Moufang", n, lib[ 3 ][ pos_n ][ m ] ); 
        SetIsCodeLoop( loop, true );
    elif name = "Steiner" then
        loop := LOOPS_ActivateSteinerLoop( n, pos_n, m );
        SetIsSteinerLoop( loop, true );
    elif name = "CC" then
        if n in [2,3,5,7] then # use Cayley table for canonical cyclic group
            loop := LoopByCayleyTable( LOOPS_DecodeCayleyTable( lib[ 3 ][ pos_n ][ m ] ) );
        else
            loop := LOOPS_ActivateCCLoop( n, pos_n, m, case );
        fi;
        SetIsCCLoop( loop, true );
    elif name = "RCC" then
        loop := LOOPS_ActivateRCCLoop( n, pos_n, m );
        SetIsRCCLoop( loop, true );
    elif name = "small" then
        loop := LoopByCayleyTable( LOOPS_DecodeCayleyTable( lib[ 3 ][ pos_n ][ m ] ) );
    elif name = "interesting" then
        if [n,m] = [96,1] then # simple Bol loop of order 96
            g := Group((1,4)(2,9)(3,10)(6,11)(7,12)(13,21)(14,22)(15,24)(16,23)(17,30)(18,29)(19,31)(20,32)(33,35)(38,40), 
                (1,2,4,6,8,7,5,3)(9,13,25,18,10,14,26,17)(11,15,27,20,12,16,28,19)(21,30,38,34,23,31,40,35)(22,32,39,36,24,29,37,33));
            h := Normalizer( g, SylowSubgroup( g, 5) );
            g := Action( g, RightCosets( g, h ), OnRight );
            loop := LoopByRightSection(Union(Filtered(ConjugacyClasses(g),c->Size(c) in [1,15,80])));
        else             
            loop := LoopByCayleyTable( LOOPS_DecodeCayleyTable( lib[ 3 ][ pos_n ][ m ][ 1 ] ) );
        fi;
        SetName( loop, lib[ 3 ][ pos_n ][ m ][ 2 ] );
    elif name = "nilpotent" then
        loop := LOOPS_ActivateNilpotentLoop( lib[ 3 ][ pos_n ][ m ] );
    elif name = "automorphic" then
        if not n in [3, 9, 27, 81] then # use Cayley table
            loop := LoopByCayleyTable( LOOPS_DecodeCayleyTable( lib[ 3 ][ pos_n ][ m ] ) );
        else # use associated left Bruck loop
            loop := LOOPS_ActivateAutomorphicLoop( n, m );
        fi;
        SetIsAutomorphicLoop( loop, true );
    elif name = "right Bruck" then
        if not n in [27,81] then # use Cayley table
            loop := LoopByCayleyTable( LOOPS_DecodeCayleyTable( lib[ 3 ][ pos_n ][ m ] ) );
        else # use cocycles
            loop := LOOPS_ActivateRightBruckLoop( n, m );
        fi;
        SetIsRightBruckLoop( loop, true );
    # up to isotopism        
    elif name = "itp small" then
        return LibraryLoop( "small", n, lib[ 3 ][ pos_n ][ m ] );
    fi;
    
    # setting the name
    if not name = "interesting" then
        SetName( loop, Concatenation( "<", name, " loop ", String( n ), "/", String( m ), ">" ) );
    fi;
    
    # returning the loop
    return loop;
end);

#############################################################################
##  READING LOOPS FROM THE LIBRARY - SPECIFIC CALLS
##  -------------------------------------------------------------------------

#############################################################################
##  
#F  LeftBolLoop( n, m ) 
#F  RightBolLoop( n, m )
#F  MoufangLoop( n, m ) 
#F  PaigeLoop( q )
#F  CodeLoop( n, m ) 
#F  SteinerLoop( n, m ) 
#F  CCLoop( n, m ) 
#F  SmallLoop( n, m ) 
#F  InterestingLoop( n, m ) 
#F  NilpotentLoop( n, m ) 
#F  AutomorphicLoop( n, m )
#F  LeftBruckLoop( n, m )
#F  RightBruckLoop( n, m )
#F  ItpSmallLoop( n, m ) 
##    

InstallGlobalFunction( LeftBolLoop, function( n, m )
    return LibraryLoop( "left Bol", n, m );
end);

InstallGlobalFunction( RightBolLoop, function( n, m )
    local loop;
    loop := Opposite( LeftBolLoop( n, m ) );
    SetIsRightBolLoop( loop, true );
    SetName( loop, Concatenation( "<right Bol loop ", String( n ), "/", String( m ), ">" ) );
    return loop;
end);

InstallGlobalFunction( MoufangLoop, function( n, m )
    return LibraryLoop( "Moufang", n, m );
end);

InstallGlobalFunction( PaigeLoop, function( q )
    # Paige loop over GF(q)
    if not q=2 then return Error("LOOPS: Only q=2 is implemented."); fi;
    return LibraryLoop( "Paige", 120, 1 );
end);

InstallGlobalFunction( CodeLoop, function( n, m )
    return LibraryLoop( "code", n, m );
end);

InstallGlobalFunction( SteinerLoop, function( n, m )
    return LibraryLoop( "Steiner", n, m );
end);

InstallGlobalFunction( CCLoop, function( n, m )
    return LibraryLoop( "CC", n, m );
end);

InstallGlobalFunction( ConjugacyClosedLoop, function( n, m )
    return LibraryLoop( "CC", n, m );
end);

InstallGlobalFunction( RCCLoop, function( n, m )
    return LibraryLoop( "RCC", n, m );
end);

InstallGlobalFunction( RightConjugacyClosedLoop, function( n, m )
    return LibraryLoop( "RCC", n, m );
end);

InstallGlobalFunction( LCCLoop, function( n, m )
    local loop;
    loop := Opposite( RCCLoop( n, m ) );
    SetIsLCCLoop( loop, true );
    SetName( loop, Concatenation( "<LCC loop ", String( n ), "/", String( m ), ">" ) );
    return loop;
end);

InstallGlobalFunction( LeftConjugacyClosedLoop, function( n, m )
    return LCCLoop( n, m );
end);

InstallGlobalFunction( SmallLoop, function( n, m )
    return LibraryLoop( "small", n, m );
end);

InstallGlobalFunction( InterestingLoop, function( n, m )
    return LibraryLoop( "interesting", n, m );
end);

InstallGlobalFunction( NilpotentLoop, function( n, m )
    return LibraryLoop( "nilpotent", n, m );
end);

InstallGlobalFunction( AutomorphicLoop, function( n, m )
    return LibraryLoop( "automorphic", n, m );
end);

InstallGlobalFunction( RightBruckLoop, function( n, m )
    return LibraryLoop( "right Bruck", n, m );
end);

InstallGlobalFunction( LeftBruckLoop, function( n, m )
    local loop;
    loop := Opposite( RightBruckLoop( n, m ) );
    SetIsLeftBruckLoop( loop, true );
    SetName( loop, Concatenation( "<left Bruck loop ", String( n ), "/", String( m ), ">" ) );
    return loop;
end);

InstallGlobalFunction( ItpSmallLoop, function( n, m )
    return LibraryLoop( "itp small", n, m );
end);
