#############################################################################
##
#W  bol_core_methods.gi     Common methods for Bol loops [loops]
##
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
##  x o y = (x*((y*y)*x))^(1/2)  (default) or
##  x o y = x^(1/2)*(y*x^(1/2))  (commented out)

InstallMethod( AssociatedLeftBruckLoop,
    [ IsLeftBolLoop ],
function( Q )
    local n, ct, squares, roots, new_ct, L, i, j;
    n := Size( Q );
    ct := CanonicalCayleyTable( CayleyTable( Q ) );
    squares := List( [ 1..n ], i -> ct[i][i] );
    if not Size( Set( squares ) ) = n then
        Error( "LOOPS: <1> must be a Bol loop in which squaring is a bijection." );
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


#############################################################################
##
#A  AssociatedRightBruckLoop( Q )
##
##  Given a right Bol loop Q for which x -> x^2 is a permutation,
##  returns the associated right Bruck loop, defined by
##  x o y = ((x*(y*y))*x)^(1/2)  (default) or
##  x o y = (x^(1/2)*y)*x^(1/2)  (commented out)

InstallMethod( AssociatedRightBruckLoop,
    [ IsRightBolLoop ],
function( Q )
    local n, ct, squares, roots, new_ct, L, i, j;
    n := Size( Q );
    ct := CanonicalCayleyTable( CayleyTable( Q ) );
    squares := List( [ 1..n ], i -> ct[i][i] );
    if not Size( Set( squares ) ) = n then
        Error( "LOOPS: <1> must be a right Bol loop in which squaring is a bijection." );
    fi;
    roots := Inverse( PermList( squares ) ); # square roots
    new_ct := List([1..n], i -> [1..n] );
    for i in [1..n] do for j in [1..n] do
        new_ct[i][j] :=  (ct[ct[j][ct[i][i]]][j])^roots;    # x o y = ((y*(x*x))*y)^(1/2)
#       new_ct[i][j] := ct[ct[j^roots][i]][j^roots];        # x o y = (y^(1/2)*x)*y^(1/2)
    od; od;
    L := LoopByCayleyTable( new_ct );   # the associated right Bruck loop
    SetIsRightBruckLoop( L, true );
    return L;
end );


#############################################################################
##
#O  IsExactGroupFactorization( G, H1, H2 )
##
##  Let G be a group and H_1, H_2 subgroups. The triple (G,H_1,H_2) is an
##  exact group factorization, if H_1 \cap H_2 = 1 and G=H_1H_2. 
##
##  Returns true if (G, H1, H2) is an exact group factorization.

InstallMethod( IsExactGroupFactorization, "for a group and two subgroups",
    [ IsGroup, IsGroup, IsGroup ],
function( G, H1, H2 )
    return IsSubgroup(G,H1) and IsSubgroup(G,H2) and Size(G)=Size(H1)*Size(H2) and IsTrivial(Intersection(H1,H2));
end);


#############################################################################
##
#F  RightBolLoopByExactGroupFactorization
##
##  Let (G,H_1,H_2) be an exact group factorization. Define
##  U = G\times G, S = H_1 \times H_2, and T = {(g,g^{-1}) : g in G }.
##  Then (U,S,T) is a right Bol loop folder 
##  Returns the right Bol loop corresponding to (U,S,T).

InstallGlobalFunction( RightBolLoopByExactGroupFactorizationNC,
function( g, h1, h2 )
    local f,sect,stab,ghom,st,q,rmlt;
	f := DirectProduct( g, g );
	sect := List( g, x -> Image( Embedding( f, 1 ), x )*Image( Embedding( f, 2 ), x^-1 ) );
	stab := ClosureGroup( Image( Embedding( f, 1 ), h1 ), Image( Embedding( f, 2 ), h2 ) );
	ghom := ActionHomomorphism( f, RightCosets( f, stab ), OnRight );
	st := Image( ghom, sect );; 
	q := LoopByRightSection( st );
	SetRightSection( q, st );
	rmlt := Subgroup( f, sect );
	SetRightMultiplicationGroup( q, Image( ghom, Subgroup( f, SmallGeneratingSet( rmlt ) ) ) );
	return q;
end); 

InstallGlobalFunction( RightBolLoopByExactGroupFactorization,
function( arg )
	local g, h1, h2;
	if Length( arg ) = 3 then
		g := arg[1]; h1 := arg[2]; h2 := arg[3];
	elif Length( arg ) = 2 then
		g := arg[1]; h1 := arg[2]; h2 := Stabilizer( g, Orbit( g )[1] );
	elif Length( arg ) = 1 and Length( arg[1] ) = 3 then 
		g := arg[1][1]; h1 := arg[1][2]; h2 := arg[1][3];
	elif Length(arg) = 1 and Length( arg[1] ) = 2 then 
		g := arg[1][1]; h1 := arg[1][2]; h2 := Stabilizer(g,Orbit(g)[1]);
	else 
		Error("LOOPS: Argument must be an exact group factorization or a group with a regular subgroup.");
	fi;
	if not IsExactGroupFactorization( g, h1, h2 ) then
	   Error("LOOPS: Argument does not correspond to an exact group factorization.");
	fi;
	return RightBolLoopByExactGroupFactorizationNC( g, h1, h2 );
end);

