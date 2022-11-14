#############################################################################
##
#W  classes.gi  Testing properties/varieties [loops]
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  ASSOCIATIVITY, COMMUTATIVITY AND GENERALIZATIONS
##  -------------------------------------------------------------------------

# (PROG) IsAssociative is already implemented for magmas, but we provide
# a new method based on sections. This new method is much faster for groups,
# and a bit slower for nonassociative loops.

InstallOtherMethod( IsAssociative, "for loops",
    [ IsLoop ], 0,
function( Q )
    local sLS, x, y;
    sLS := Set( LeftSection( Q ) );
    for x in LeftSection( Q ) do
        for y in LeftSection( Q ) do
            if not x*y in sLS then return false; fi;
        od;
    od;
    return true;
end);

# implies
# InstallTrueMethod( IsExtraLoop, IsAssociative and IsLoop );

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

#############################################################################
##
#P  IsPowerAssociative( Q )
##
##  Returns true if <Q> is a power associative quasigroup.

InstallOtherMethod( IsPowerAssociative, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local checked, x, S;
    checked := [];
    repeat
        x := Difference( Elements(Q), checked );
        if IsEmpty( x ) then
            return true;
        fi;
        S := Subquasigroup( Q, [x[1]] );
        if not IsAssociative( S ) then
            return false;
        fi;
        checked := Union( checked, Elements( S ) ); # S is a group, so every subquasigroup of S is a group
    until 0=1;
end );

# implies
# InstallTrueMethod( HasTwosidedInverses, IsPowerAssociative and IsLoop );

#############################################################################
##
#P  IsDiassociative( Q )
##
##  Returns true if <Q> is a diassociative quasigroup.

InstallOtherMethod( IsDiassociative, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local checked, all_pairs, x, S;
    checked := [];
    all_pairs := Combinations( PosInParent( Elements( Q ) ), 2 ); # it is faster to work with integers
    repeat
        x := Difference( all_pairs, checked );
        if IsEmpty( x ) then
            return true;
        fi;
        S := Subquasigroup( Q, x[1] );
        if not IsAssociative( S ) then
            return false;
        fi;
        checked := Union( checked, Combinations( PosInParent( Elements( S ) ), 2 ) );
    until 0=1;
end );

# implies
# InstallTrueMethod( IsPowerAlternative, IsDiassociative );
# InstallTrueMethod( IsFlexible, IsDiassociative );

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
        LeftInverse( x*y ) = LeftInverse( y )*LeftInverse( x ) ) );
end );

# implies and is implied by (for inverse properties)
# InstallTrueMethod( HasAntiautomorphicInverseProperty, HasAutomorphicInverseProperty and IsCommutative );
# InstallTrueMethod( HasAutomorphicInverseProperty, HasAntiautomorphicInverseProperty and IsCommutative );
# InstallTrueMethod( HasLeftInverseProperty, HasInverseProperty );
# InstallTrueMethod( HasRightInverseProperty, HasInverseProperty );
# InstallTrueMethod( HasWeakInverseProperty, HasInverseProperty );
# InstallTrueMethod( HasAntiautomorphicInverseProperty, HasInverseProperty );
# InstallTrueMethod( HasTwosidedInverses, HasAntiautomorphicInverseProperty );
# InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and IsCommutative );
# InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and IsCommutative );
# InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasRightInverseProperty );
# InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasWeakInverseProperty );
# InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and HasWeakInverseProperty );
# InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasAntiautomorphicInverseProperty );
# InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and HasAntiautomorphicInverseProperty );
# InstallTrueMethod( HasInverseProperty, HasWeakInverseProperty and HasAntiautomorphicInverseProperty );
# InstallTrueMethod( HasTwosidedInverses, HasLeftInverseProperty );
# InstallTrueMethod( HasTwosidedInverses, HasRightInverseProperty );
# InstallTrueMethod( HasTwosidedInverses, IsFlexible and IsLoop );


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
##  LOOPS OF BOL-MOUFANG
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
# InstallTrueMethod( IsMoufangLoop, IsExtraLoop );
# InstallTrueMethod( IsCLoop, IsExtraLoop );

# is implied by
# InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsLeftNuclearSquareLoop );
# InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsMiddleNuclearSquareLoop );
# InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsRightNuclearSquareLoop );

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
# InstallTrueMethod( IsLeftBolLoop, IsMoufangLoop );
# InstallTrueMethod( IsRightBolLoop, IsMoufangLoop );
# InstallTrueMethod( IsDiassociative, IsMoufangLoop );

# is implied by
# InstallTrueMethod( IsMoufangLoop, IsLeftBolLoop and IsRightBolLoop );

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
# InstallTrueMethod( IsLCLoop, IsCLoop );
# InstallTrueMethod( IsRCLoop, IsCLoop );
# InstallTrueMethod( IsDiassociative, IsCLoop and IsFlexible);

# is implied by
# InstallTrueMethod( IsCLoop, IsLCLoop and IsRCLoop );

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
# InstallTrueMethod( IsRightBolLoop, IsLeftBolLoop and IsCommutative );
# InstallTrueMethod( IsLeftPowerAlternative, IsLeftBolLoop );

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
# InstallTrueMethod( IsLeftBolLoop, IsRightBolLoop and IsCommutative );
# InstallTrueMethod( IsRightPowerAlternative, IsRightBolLoop );

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
# InstallTrueMethod( IsLeftPowerAlternative, IsLCLoop );
# InstallTrueMethod( IsLeftNuclearSquareLoop, IsLCLoop );
# InstallTrueMethod( IsMiddleNuclearSquareLoop, IsLCLoop );
# InstallTrueMethod( IsRCLoop, IsLCLoop and IsCommutative );

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
# InstallTrueMethod( IsRightPowerAlternative, IsRCLoop );
# InstallTrueMethod( IsRightNuclearSquareLoop, IsRCLoop );
# InstallTrueMethod( IsMiddleNuclearSquareLoop, IsRCLoop );
# InstallTrueMethod( IsLCLoop, IsRCLoop and IsCommutative );

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

#implies
# InstallTrueMethod( IsRightNuclearSquareLoop, IsLeftNuclearSquareLoop and IsCommutative );

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

# implies
# InstallTrueMethod( IsLeftNuclearSquareLoop, IsRightNuclearSquareLoop and IsCommutative );

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
# InstallTrueMethod( IsLeftNuclearSquareLoop, IsNuclearSquareLoop );
# InstallTrueMethod( IsRightNuclearSquareLoop, IsNuclearSquareLoop );
# InstallTrueMethod( IsMiddleNuclearSquareLoop, IsNuclearSquareLoop );

# is implied by
# InstallTrueMethod( IsNuclearSquareLoop, IsLeftNuclearSquareLoop
#    and IsRightNuclearSquareLoop and IsMiddleNuclearSquareLoop );

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

# is implied by
# InstallTrueMethod( IsFlexible, IsCommutative );

#############################################################################
##
#P  IsLeftAlternative( Q )
##
##  Returns true if <Q> is a left alternative quasigroup.

InstallMethod( IsLeftAlternative, "for quasigroup",
    [ IsQuasigroup],
function( Q )
    if IsLoop( Q ) then 
        return ForAll( LeftSection( Q ), a -> a*a in LeftSection( Q ) );
    fi;
    return ForAll( Q, x -> ForAll( Q, y -> x*(x*y) = (x*x)*y ) );
end );

# implies
# InstallTrueMethod( IsRightAlternative, IsLeftAlternative and IsCommutative );

#############################################################################
##
#P  IsRightAlternative( Q )
##
##  Returns true if <Q> is a right alternative quasigroup.

InstallMethod( IsRightAlternative, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    if IsLoop( Q ) then
        return ForAll( RightSection( Q ), a -> a*a in RightSection( Q ) );
    fi;
    return ForAll( Q, x -> ForAll( Q, y -> (x*y)*y = x*(y*y) ) );
end );

# implies
# InstallTrueMethod( IsLeftAlternative, IsRightAlternative and IsCommutative );

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
# InstallTrueMethod( IsLeftAlternative, IsAlternative );
# InstallTrueMethod( IsRightAlternative, IsAlternative );

# is implied by
# InstallTrueMethod( IsAlternative, IsLeftAlternative and IsRightAlternative );

#############################################################################
##  POWER ALTERNATIVE LOOPS
##  -------------------------------------------------------------------------

#############################################################################
##
#P  IsLeftPowerAlternative( L )
##
##  Returns true if <L> is a left power alternative loop.

InstallMethod( IsLeftPowerAlternative, "for loop",
    [ IsLoop ],
function( L )
    local i, M;
    if Size( L ) = 1 then return true; fi;
    for i in [ 2..Size( L )] do
        M := Subloop( L, [ Elements( L )[ i ] ]);
        if not Size( RelativeLeftMultiplicationGroup( L, M ) ) = Size( M ) then
            return false;
        fi;
    od;
    return true;
end );

# implies
# InstallTrueMethod( IsLeftAlternative, IsLeftPowerAlternative );
# InstallTrueMethod( HasLeftInverseProperty, IsLeftPowerAlternative );
# InstallTrueMethod( IsPowerAssociative, IsLeftPowerAlternative );

#############################################################################
##
#P  IsRightPowerAlternative( L )
##
##  Returns true if <L> is a right power alternative loop.

InstallMethod( IsRightPowerAlternative, "for loop",
    [ IsLoop ],
function( L )
    local i, M;
    if Size( L ) = 1 then return true; fi;
    for i in [ 2..Size( L ) ] do
        M := Subloop( L, [ Elements( L )[ i ] ] );
        if not Size( RelativeRightMultiplicationGroup( L, M ) ) = Size( M ) then
            return false;
        fi;
    od;
    return true;
end );

# implies
# InstallTrueMethod( IsRightAlternative, IsRightPowerAlternative );
# InstallTrueMethod( HasRightInverseProperty, IsRightPowerAlternative );
# InstallTrueMethod( IsPowerAssociative, IsRightPowerAlternative );

#############################################################################
##
#P  IsPowerAlternative( L )
##
##  Returns true if <L> is a power alternative loop.

InstallMethod( IsPowerAlternative, "for loop",
    [ IsLoop ],
function( L )
    return ( IsLeftPowerAlternative( L ) and IsRightPowerAlternative( L ) );
end );

# implies
# InstallTrueMethod( IsLeftPowerAlternative, IsPowerAlternative );
# InstallTrueMethod( IsRightPowerAlternative, IsPowerAlternative );

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

# implies
# InstallTrueMethod( IsAssociative, IsLCCLoop and IsCommutative );
# InstallTrueMethod( IsExtraLoop, IsLCCLoop and IsMoufangLoop );

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

# implies
# InstallTrueMethod( IsAssociative, IsRCCLoop and IsCommutative );
# InstallTrueMethod( IsExtraLoop, IsRCCLoop and IsMoufangLoop );

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
# InstallTrueMethod( IsLCCLoop, IsCCLoop );
# InstallTrueMethod( IsRCCLoop, IsCCLoop );

# is implied by
# InstallTrueMethod( IsCCLoop, IsLCCLoop and IsRCCLoop );

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
# InstallTrueMethod( IsOsbornLoop, IsMoufangLoop );
# InstallTrueMethod( IsOsbornLoop, IsCCLoop );

#############################################################################
##  ADDITIONAL VARIETIES OF LOOPS
##  -------------------------------------------------------------------------

#############################################################################
##
#P  IsCodeLoop( L )
##
##  Returns true if <L> is an even code loop.

InstallMethod( IsCodeLoop, "for loop",
    [ IsLoop ],
function( L )
    # even code loops are precisely Moufang 2-loops with Frattini subloop of order 1, 2
    return Set( Factors( Size( L ) ) ) = [ 2 ]
        and IsMoufangLoop( L )
        and Size( FrattiniSubloop( L ) ) in [1, 2];
end );

# implies
# InstallTrueMethod( IsExtraLoop, IsCodeLoop );
# InstallTrueMethod( IsCCLoop, IsCodeLoop );

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
# InstallTrueMethod( IsCommutative, IsSteinerLoop );
# InstallTrueMethod( IsCLoop, IsSteinerLoop );

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
# InstallTrueMethod( HasAutomorphicInverseProperty, IsLeftBruckLoop );
# InstallTrueMethod( IsLeftBolLoop, IsLeftBruckLoop );
# InstallTrueMethod( IsRightBruckLoop, IsLeftBruckLoop and IsCommutative );

# is implied by
# InstallTrueMethod( IsLeftBruckLoop, IsLeftBolLoop and HasAutomorphicInverseProperty );

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
# InstallTrueMethod( HasAutomorphicInverseProperty, IsRightBruckLoop );
# InstallTrueMethod( IsRightBolLoop, IsRightBruckLoop );
# InstallTrueMethod( IsLeftBruckLoop, IsRightBruckLoop and IsCommutative );

# is implied by
# InstallTrueMethod( IsRightBruckLoop, IsRightBolLoop and HasAutomorphicInverseProperty );

#############################################################################
##
#P  IsLeftALoop( L )
##
##  Returns true if <L> is a left A-loop, that is if
##  all left inner mappings are automorphisms of <L>.

InstallMethod( IsLeftALoop, "for loop",
   [ IsLoop ],
function( L )
   local gens;
   gens := GeneratorsOfGroup( LeftInnerMappingGroup( L ) );
   return ForAll(gens, f -> ForAll(L, x -> ForAll(L, y -> (x * y)^f = x^f * y^f )));
end);

#############################################################################
##
#P  IsRightALoop( L )
##
##  Returns true if <L> is a right A-loop, that is if
##  all right inner mappings are automorphisms of <L>.

InstallMethod( IsRightALoop, "for loop",
   [ IsLoop ],
function( L )
   local gens;
   gens := GeneratorsOfGroup( RightInnerMappingGroup( L ) );
   return ForAll(gens, f -> ForAll(L, x -> ForAll(L, y -> (x * y)^f = x^f * y^f )));
end);

#############################################################################
##
#P  IsMiddleALoop( L )
##
##  Returns true if <L> is a middle A-loop, that is if
##  all middle inner mappings (conjugations) are automorphisms of <L>.

InstallMethod( IsMiddleALoop, "for loop",
   [ IsLoop ],
function( L )
   local gens;
   gens := GeneratorsOfGroup( MiddleInnerMappingGroup( L ) );
   return ForAll(gens, f -> ForAll(L, x -> ForAll(L, y -> (x * y)^f = x^f * y^f )));
end);

#############################################################################
##
#P  IsALoop( L )
##
##  Returns true if <L> is an A-loop, that is if
##  all inner mappings are automorphisms of <L>.

InstallMethod( IsALoop, "for loop",
   [ IsLoop ],
function( Q )
   return IsRightALoop(Q) and IsMiddleALoop(Q); 
   # Theorem: right A-loop + middle A-loop implies left A-loop
end);

# implies
# InstallTrueMethod( IsLeftALoop, IsALoop );
# InstallTrueMethod( IsRightALoop, IsALoop );
# InstallTrueMethod( IsMiddleALoop, IsALoop );
# InstallTrueMethod( IsLeftALoop, IsRightALoop and HasAntiautomorphicInverseProperty );
# InstallTrueMethod( IsRightALoop, IsLeftALoop and HasAntiautomorphicInverseProperty );
# InstallTrueMethod( IsFlexible, IsMiddleALoop );
# InstallTrueMethod( HasAntiautomorphicInverseProperty, IsFlexible and IsLeftALoop );
# InstallTrueMethod( HasAntiautomorphicInverseProperty, IsFlexible and IsRightALoop );
# InstallTrueMethod( IsMoufangLoop, IsALoop and IsLeftAlternative );
# InstallTrueMethod( IsMoufangLoop, IsALoop and IsRightAlternative );
# InstallTrueMethod( IsMoufangLoop, IsALoop and HasLeftInverseProperty );
# InstallTrueMethod( IsMoufangLoop, IsALoop and HasRightInverseProperty );
# InstallTrueMethod( IsMoufangLoop, IsALoop and HasWeakInverseProperty );

# is implied by
# InstallTrueMethod( IsMiddleALoop, IsCommutative and IsLoop);
# InstallTrueMethod( IsLeftALoop, IsLeftBruckLoop );
# InstallTrueMethod( IsLeftALoop, IsLCCLoop );
# InstallTrueMethod( IsRightALoop, IsRightBruckLoop );
# InstallTrueMethod( IsRightALoop, IsRCCLoop );
# InstallTrueMethod( IsALoop, IsCommutative and IsMoufangLoop );
# InstallTrueMethod( IsALoop, IsLeftALoop and IsMiddleALoop );
# InstallTrueMethod( IsALoop, IsRightALoop and IsMiddleALoop );
# InstallTrueMethod( IsALoop, IsAssociative and IsLoop);
