#############################################################################
##
#W  core_methods.tst   Testing core methods      G. P. Nagy / P. Vojtechovsky
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##
gap> START_TEST("LOOPS, core_methods: testing core methods");

# TESTING VIEW AND PRINT MODE, AND LATIN SQUARE FUNCTIONS
gap> T := [ [ 2, 1 ], [ 1, 2 ] ];;
gap> IsQuasigroupTable( T );
true
gap> IsLoopTable( T );
false
gap> Q := QuasigroupByCayleyTable( T );
<quasigroup of order 2>
gap> Elements( Q );
[ q1, q2 ]
gap> L := IntoLoop( Q );
<loop of order 2>
gap> Elements( L );
[ l1, l2 ]
gap> L.1;
l1

# TESTING MORE CONVERSION FUNCTIONS
gap> G := IntoGroup( Q );
Group([ (), (1,2) ])
gap> G := IntoGroup( L );
Group([ (), (1,2) ])
gap> IntoQuasigroup( G );
<quasigroup of order 2>
gap> IntoLoop( G );
<loop of order 2>
gap> IntoLoop( Group( (1,2,3), (1,2), (1,4) ) );
<loop of order 24>
gap> PrincipalLoopIsotope( Q, Elements(Q)[1], Elements(Q)[1] );
<loop of order 2>
gap> CanonicalCopy( QuasigroupByCayleyTable( [[2,3],[3,2]] ) );
<quasigroup of order 2>

# TESTING DIRECT PRODUCTS AND OPPOSITES
gap> L := MoufangLoop( 12, 1 );;
gap> DirectProduct( L );
<Moufang loop 12/1>
gap> DirectProduct( L, L, L );
<loop of order 1728>
gap> DirectProduct( L, Group( (1,2,3) ) );
<loop of order 36>
gap> DirectProduct( Group( (1,2,3) ), L, Group( (1,2,3,4) ) );
<loop of order 144>
gap> Q := QuasigroupByCayleyTable([[2,1],[1,2]]);;
gap> DirectProduct( Q, Q );
<quasigroup of order 4>
gap> DirectProduct( Q, L, Group((1,2)) );
<quasigroup of order 48>
gap> OppositeLoop( L );
<loop of order 12>

# TESTING BASIC ATTRIBUTES
gap> One( L );
l1
gap> Size( L );
12
gap> CayleyTable( Q );
[ [ 2, 1 ], [ 1, 2 ] ]
gap> Exponent( L );
6
gap> Opposite( L );
<loop of order 12>

# TESTING BASIC ARITHMETIC OPERATIONS
gap> eL := Elements( L );;
gap> eL[ 2 ]*eL[ 3 ]*eL[ 7 ] = (eL[ 2 ]*eL[ 3 ])* eL[ 7 ];
true
gap> eL[ 2 ]*eL[ 3 ]*eL[ 7 ] = eL[ 2 ]*(eL[ 3 ]* eL[ 7 ]);
false
gap> LeftDivision( eL[ 2 ], eL[ 3 ] );
l4
gap> RightDivision( eL[ 2 ], eL[ 3 ] );
l6
gap> LeftInverse( eL[ 2 ] ) = RightInverse( eL[ 2 ] );
true
gap> Associator( eL[ 2 ], eL[ 3 ], eL[ 4 ] );
l1
gap> Commutator( eL[ 2 ], eL[ 3 ] );
l5

# TESTING GENERATORS
gap> GeneratorsOfLoop( L );
[ l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12 ]
gap> GeneratorsSmallest( L );
[ l10, l11, l12 ]
gap> SmallGeneratingSet( L );
[ l2, l3, l7 ]

# TESTING SECTIONS AND TRANSLATIONS
gap> LeftSection( L );
[ (), (1,2)(3,4)(5,6)(7,8)(9,12)(10,11), (1,3,5)(2,6,4)(7,9,11)(8,10,12), 
  (1,4)(2,5)(3,6)(7,10)(8,9)(11,12), (1,5,3)(2,4,6)(7,11,9)(8,12,10), 
  (1,6)(2,3)(4,5)(7,12)(8,11)(9,10), (1,7)(2,8)(3,11)(4,10)(5,9)(6,12), 
  (1,8)(2,7)(3,12)(4,9)(5,10)(6,11), (1,9)(2,12)(3,7)(4,8)(5,11)(6,10), 
  (1,10)(2,11)(3,8)(4,7)(5,12)(6,9), (1,11)(2,10)(3,9)(4,12)(5,7)(6,8), 
  (1,12)(2,9)(3,10)(4,11)(5,8)(6,7) ]
gap> RightSection( L );
[ (), (1,2)(3,6)(4,5)(7,8)(9,12)(10,11), (1,3,5)(2,4,6)(7,11,9)(8,12,10), 
  (1,4)(2,3)(5,6)(7,10)(8,9)(11,12), (1,5,3)(2,6,4)(7,9,11)(8,10,12), 
  (1,6)(2,5)(3,4)(7,12)(8,11)(9,10), (1,7)(2,8)(3,9)(4,10)(5,11)(6,12), 
  (1,8)(2,7)(3,10)(4,9)(5,12)(6,11), (1,9)(2,12)(3,11)(4,8)(5,7)(6,10), 
  (1,10)(2,11)(3,12)(4,7)(5,8)(6,9), (1,11)(2,10)(3,7)(4,12)(5,9)(6,8), 
  (1,12)(2,9)(3,8)(4,11)(5,10)(6,7) ]
gap> LeftTranslation( L, eL[ 3 ] );
(1,3,5)(2,6,4)(7,9,11)(8,10,12)
gap> RightTranslation( L, eL[ 3 ] );
(1,3,5)(2,4,6)(7,11,9)(8,12,10)

# TESTING MULTIPLICATION GROUPS AND INNER MAPPING GROUPS
gap> LeftMultiplicationGroup( L );
<permutation group with 12 generators>
gap> RightMultiplicationGroup( L );
<permutation group with 12 generators>
gap> Size( MultiplicationGroup( L ) );
2592
gap> Size( InnerMappingGroup( L ) );
216
gap> MiddleInnerMappingGroup( L );
<permutation group with 12 generators>

# TESTING LOOP BY LEFT/RIGHT SECTION
gap> LoopByRightSection( [ (), (1,2)(3,4,5), (1,3,5)(2,4), (1,4,3)(2,5), (1,5,4)(2,3) ] );
<loop of order 5>
gap> S := Subloop( MoufangLoop( 12, 1 ), [ 3 ] );;
gap> LoopByLeftSection( LeftSection( S ) );
<loop of order 3>
gap> LoopByRightSection( RightSection( S ) );
<loop of order 3>
gap> QuasigroupByLeftSection( LeftSection( S ) );
<quasigroup of order 3>
gap> QuasigroupByRightSection( RightSection( S ) );
<quasigroup of order 3>
gap> CayleyTableByPerms( LeftSection( S ) );
[ [ 1, 3, 5 ], [ 3, 5, 1 ], [ 5, 1, 3 ] ]

# TESTING LOOP BY RIGHT FOLDER 
gap> LOOPS_Shift := function( p )
>       local ls;
>       ls := ListPerm( p );
>       ls := Concatenation( [1,2,3,4,5], List( ls, x -> x + 5 ) );
>       return PermList( ls );
> end;
function( p ) ... end
gap> A := AlternatingGroup( 5 );
Alt( [ 1 .. 5 ] )
gap> G := DirectProduct( A, A );
Group([ (1,2,3,4,5), (3,4,5), (6,7,8,9,10), (8,9,10) ])
gap> H := Subgroup( G, [ (1,2,3), (2,3,4), (6,7,8,9,10) ] );
Group([ (1,2,3), (2,3,4), (6,7,8,9,10) ])
gap> T := List( A, x -> x * LOOPS_Shift(x)^(-1) );;
gap> LoopByRightFolder( G, H, T );
<loop of order 60>
gap> QuasigroupByRightFolder( G, H, T );
<quasigroup of order 60>

# TESTING RANDOM QUASIGROUPS AND LOOPS
gap> RandomQuasigroup( 10 );
<quasigroup of order 10>
gap> RandomQuasigroup( 10, 100 );
<quasigroup of order 10>
gap> RandomLoop( 11 );
<loop of order 11>
gap> RandomLoop( 30, 1000 );
<loop of order 30>
gap> RandomNilpotentLoop( [2, 3, 5] );
<loop of order 30>
gap> RandomNilpotentLoop( [2, CyclicGroup(3), 6] );
<loop of order 36>

# TESTING SUBQUASIGROUPS AND SUBLOOPS
gap> L := MoufangLoop( 32, 5 );;
gap> Length( AllSubloops( L ) );
90
gap> S := Subloop( L, [ Elements( L )[ 2 ], Elements( L )[ 25 ] ] );
<loop of order 8>
gap> L = Parent( S );
true
gap> L = Parent( L );
true
gap> PosInParent( S );
[ 1, 2, 5, 8, 22, 25, 29, 31 ]
gap> IsSubloop( L, S );
true
gap> CayleyTable( S );
[ [ 1, 2, 5, 8, 22, 25, 29, 31 ], [ 2, 5, 8, 1, 31, 22, 25, 29 ], 
  [ 5, 8, 1, 2, 29, 31, 22, 25 ], [ 8, 1, 2, 5, 25, 29, 31, 22 ], 
  [ 22, 25, 29, 31, 1, 2, 5, 8 ], [ 25, 29, 31, 22, 8, 1, 2, 5 ], 
  [ 29, 31, 22, 25, 5, 8, 1, 2 ], [ 31, 22, 25, 29, 2, 5, 8, 1 ] ]
gap> LeftTranslation( S, Elements( S )[ 2 ] );  
(1,2,5,8)(22,31,29,25)
gap> SS := Subloop( S, [ Elements( S )[ 2 ] ] );
<loop of order 4>
gap> Parent( SS ) = L;
true
gap> CayleyTable( SS );
[ [ 1, 2, 5, 8 ], [ 2, 5, 8, 1 ], [ 5, 8, 1, 2 ], [ 8, 1, 2, 5 ] ]
gap> IsSubloop( S, SS );
true
gap> AllSubquasigroups( QuasigroupByCayleyTable( [[2,1],[1,2]] ) );
[ <quasigroup of order 2>, <quasigroup of order 1> ]

# TESTING NUCLEUS, COMMUTANT, CENTER
gap> LeftNucleus( L ) = NucleusOfLoop( L );
true
gap> MiddleNucleus( L ) = RightNucleus( L );
true
gap> Commutant( L );
[ l1, l4, l5, l11 ]
gap> Center( L );
<associative loop of order 4>
gap> AssociatorSubloop( L );
<loop of order 2>

# TESTING COMMUTATIVITY AND GENERALIZATIONS
gap> IsAssociative( L );
false
gap> IsCommutative( L );
false
gap> IsCommutative( Q );
true
gap> IsPowerAssociative( L );
true
gap> IsDiassociative( L );
true

# TESTING INVERSE PROPERTIES
gap> B := LeftBolLoop( 8, 1 );;
gap> HasLeftInverseProperty( B );
true
gap> HasRightInverseProperty( B );
false
gap> HasInverseProperty( B );
false
gap> HasTwosidedInverses( B );
true
gap> HasAutomorphicInverseProperty( B );
true
gap> HasAntiautomorphicInverseProperty( B );
false

# TESTING PROPERTIES OF QUASIGROUPS
gap> IsSemisymmetric( Q );
true
gap> IsTotallySymmetric( Q );
true
gap> IsTotallySymmetric( B );
false
gap> IsIdempotent( Q );
false
gap> IsSteinerQuasigroup( Q );
false
gap> IsUnipotent( Q );
true
gap> IsLeftDistributive( B );
false
gap> IsRightDistributive( B );
false
gap> IsDistributive( B );
false
gap> IsEntropic( Q );
true
gap> IsMedial( Q );
true

# TESTING LOOPS OF BOL-MOUFANG TYPE
gap> L := DirectProduct( MoufangLoop( 12, 1 ), Group( (1,2)(3,4), (1,3)(2,4) ) );
<loop of order 48>
gap> IsLeftAlternative( L );
true
gap> IsRightAlternative( L );
true
gap> L;
<alternative loop of order 48>
gap> IsLeftNuclearSquareLoop( L );
false
gap> IsRightNuclearSquareLoop( L );
false
gap> IsMiddleNuclearSquareLoop( L );
false
gap> IsNuclearSquareLoop( L );
false
gap> IsLCLoop( L );
false
gap> IsRCLoop( L );
false
gap> IsCLoop( L );
false
gap> IsFlexible( L );
true
gap> IsLeftBolLoop( L );
true
gap> L;
<left Bol loop of order 48>
gap> IsRightBolLoop( L );
true
gap> L;
<Moufang loop of order 48>
gap> IsExtraLoop( L );
false

# TESTING CONJUGACY CLOSED LOOPS
gap> IsLCCLoop( L ); IsLeftConjugacyClosedLoop( L );
false
false
gap> IsRCCLoop( L ); IsRightConjugacyClosedLoop( L );
false
false
gap> IsCCLoop( L ); IsConjugacyClosedLoop( L );
false
false

# TESTING BRUCK AND STEINER LOOPS
gap> IsLeftBruckLoop( B );
true
gap> IsRightBruckLoop( B );
false
gap> IsLeftKLoop( B );
true
gap> IsRightKLoop( B );
false
gap> IsSteinerLoop( B );
false

# TESTING A-LOOPS
gap> IsLeftALoop( B );
true
gap> IsRightALoop( B );
true
gap> IsMiddleALoop( B );
false
gap> IsALoop( B );
false

# TESTING NORMALITY
gap> L := MoufangLoop( 32, 27 );;
gap> S := Subloop( L, [ L.3, L.4 ] );;
gap> IsNormal( L, S );
true
gap> FactorLoop( L, S );
<loop of order 4>
gap> NaturalHomomorphismByNormalSubloop( L, S );
MappingByFunction( <Moufang loop 32/27>, <loop of order 
4>, function( x ) ... end )
gap> S := Subloop( L, [ Elements( L )[ 7 ] ] );;
gap> IsNormal( L, S );
false
gap> NormalClosure( L, S );
<loop of order 8>

# TESTING NILPOTENCY (MORE TESTING IN FILE nilpot.tst)
gap> IsNilpotent( L );
true
gap> IsStronglyNilpotent( L );
true
gap> UpperCentralSeries( L );
[ <loop of order 32>, <loop of order 4>, <loop of order 2>, 
  <associative loop of order 1> ]
gap> LowerCentralSeries( L );
[ <Moufang loop 32/27>, <loop of order 4>, <loop of order 2>, 
  <associative loop of order 1> ]
gap> NilpotencyClassOfLoop( L );
3

# TESTING SOLVABILITY
gap> IsSolvable( L );
true
gap> DerivedSubloop( L );
<loop of order 4>
gap> DerivedLength( L );
2
gap> FrattiniSubloop( L );
<loop of order 4>
gap> FrattinifactorSize( L );
8

#
gap> STOP_TEST( "core_methods.tst", 10000000 );
