# Version 31/12/2017
# https://en.wikipedia.org/wiki/Quasigroup
###################################################################

InstallGlobalFunction( DG4L_LatinSquare2LSDesign_NC, function( ct )
	local n, i, j, bls;
	n := Size( ct );
	bls := [ ];
	for i in [ 1..n ] do
		for j in [ 1..n ] do
			Add( bls, [ i, n+j, 2*n+ct[i][j] ] );
		od;
	od;
	return bls;
end );

InstallGlobalFunction( DG4L_LSDesign2LatinSquare_NC, function( bls )
	local n, ct, b;
	n := RootInt( Length( bls ) );
	ct := List( [1..n], i -> [] );
	for b in bls do
		ct[b[1]][b[2]-n] := b[3]-2*n;
	od;
	return ct;
end );

InstallGlobalFunction( DG4L_LatinSquare2Digraph_NC, function( ct ) 
	local n, bls, dg;
	n := Size( ct );
	bls := DG4L_LatinSquare2LSDesign_NC( ct );
	dg := Digraph( Concatenation( [1..3*n], bls ), function(x,y) return IsPosInt(x) and IsList(y) and x in y; end );
	SetOrder( dg, n );
	return dg;
end );

InstallGlobalFunction( DG4L_IsostrophismOnLatinSquare_NC, function( ct, p ) 
	local bls;
	bls := DG4L_LatinSquare2LSDesign_NC( ct );
	return DG4L_LSDesign2LatinSquare_NC( OnSetsSets( bls, p ) );
end );

InstallGlobalFunction( DG4L_NiceColoring_NC, function( Q ) 
    local n, bls, refl_map, rotations, coloring;
    n := Size(Q);
    bls := DG4L_LatinSquare2LSDesign_NC( MultiplicationTable( Q ) );
    refl_map := x -> Product(List(Filtered(bls,b->x in b),b->Difference(b,[x])),b->(b[1],b[2]));
    rotations := List( bls, b -> CycleStructurePerm( Product( b, refl_map ) ) );
    coloring := Set( rotations );
    coloring := List( rotations, x -> Position(coloring,x)); 
    return coloring;
end );

###################################################################

InstallMethod( IsIsotopismPerm, "for a permutation and an integer",
    [ IsPerm, IsPosInt ],
function( p, n )
	return [[1..n],n+[1..n],2*n+[1..n]]=OnTuplesSets([[1..n],n+[1..n],2*n+[1..n]],p);
end );

InstallMethod( IsIsostrophismPerm, "for a permutation and an integer",
    [ IsPerm, IsPosInt ],
function( p, n )
	return [[1..n],n+[1..n],2*n+[1..n]]=OnSetsSets([[1..n],n+[1..n],2*n+[1..n]],p);
end );

###################################################################

InstallMethod( OnQuasigroupsByIsostrophism, "for a quasigroup and a permutation of [1..3n]",
    [ IsQuasigroup, IsPerm ],
function( Q, p )
	local n, ct;
	n := Size( Q );
	if not IsIsostrophismPerm( p, n ) then 
		Error("2nd argument must be a permutation representing an isostrophism");
	fi;
	ct := DG4L_IsostrophismOnLatinSquare_NC( MultiplicationTable( Q ), p );
	if IsLoopTable( ct ) then 
		return LoopByCayleyTable( ct );
	else
		return QuasigroupByCayleyTable( ct );
	fi;
end );

InstallMethod( OnQuasigroupsByIsotopism, "for a quasigroup and a permutation of [1..3n]",
    [ IsQuasigroup, IsPerm ],
function( Q, p )
	local n;
	n := Size( Q );
	if not IsIsotopismPerm( p, n ) then 
		Error("2nd argument must be a permutation representing an isotopism");
	fi;
	return OnQuasigroupsByIsostrophism( Q, p );
end );

InstallMethod( OnQuasigroupsByIsomorphism, "for a quasigroup and a permutation of [1..n]",
    [ IsQuasigroup, IsPerm ],
function( Q, p )
	local n, pp;
	n := Size( Q );
	if [1..n] <> OnSets( [1..n], p ) then
		Error( "second argument must be a permutation representing an isomorphism" );
	fi;
	pp := Permuted( [1..n], p );
	pp := Concatenation( pp, n+pp, 2*n+pp );
	return OnQuasigroupsByIsostrophism( Q, PermList( pp ) );
end );

InstallMethod( OnQuasigroupsByConjugation, "for a quasigroup and a permutation of [1,2,3]",
    [ IsQuasigroup, IsPerm ],
function( Q, p )
	local n, pp;
	if not p in [(),(1,2),(1,3),(2,3),(1,2,3),(1,3,2)] then
		Error( "second argument must be a permutation of [1,2,3]" );
	fi;
	n := Size( Q );
	pp := Concatenation( Permuted( [[1..n],n+[1..n],2*n+[1..n]], p ) );
	return OnQuasigroupsByIsostrophism( Q, PermList( pp ) );
end );

###################################################################

InstallMethod( AutostrophismGroup, "for a quasigroup",
    [ IsQuasigroup ],
function( Q )
	local n, dg, cols;
	n := Size( Q );
	dg := DG4L_LatinSquare2Digraph_NC( MultiplicationTable( Q ) );
#	cols := Concatenation( List([1..3*n],i->1), List([1..n^2],i->2) );
	cols := CanonicalIsostropheColoring( Q );
	return Action(AutomorphismGroup(dg,cols),[1..3*n]);
end );

InstallMethod( AutotopismGroup, "for a quasigroup",
    [ IsQuasigroup ],
function( Q )
	local n, dg, cols;
	n := Size( Q );
	dg := DG4L_LatinSquare2Digraph_NC( MultiplicationTable( Q ) );
#	cols := Concatenation( List([1..n],i->1), List([1..n],i->2), List([1..n],i->3), List([1..n^2],i->4) );
	cols := CanonicalIsotopeColoring( Q );
	return Action(AutomorphismGroup(dg,cols),[1..3*n]);
end );

InstallMethod( CanonicalIsostropheColoring, "for a quasigroup",
    [ IsQuasigroup ],
function( Q )
	if Size( Q ) <= 50 or IsQuasigroupForNiceColoring( Q ) then
		return Concatenation( List([1..3*Size(Q)],i->1), 1+DG4L_NiceColoring_NC( Q ) );;
	else
		return Concatenation( List([1..3*Size(Q)],i->1), 1+List([1..Size(Q)^2],i->1) );
	fi;
end );

InstallMethod( CanonicalIsotopeColoring, "for a quasigroup",
    [ IsQuasigroup ],
function( Q )
	local n;
	n := Size( Q );
	if Size( Q ) <= 50 or IsQuasigroupForNiceColoring( Q ) then
		return Concatenation( List([1..n],i->1), List([1..n],i->2), List([1..n],i->3), 3+DG4L_NiceColoring_NC( Q ) );;
	else
		return Concatenation( List([1..n],i->1), List([1..n],i->2), List([1..n],i->3), 3+List([1..n^2],i->1) );
	fi;
end );

InstallMethod( CanonicalIsostropheLabelling, "for a quasigroup",
    [ IsQuasigroup ],
function( Q )
	local dg, perm;
	dg := DG4L_LatinSquare2Digraph_NC( MultiplicationTable( Q ) );
	perm := BlissCanonicalLabelling( dg, CanonicalIsostropheColoring( Q ) ); 
	return perm;
end );

InstallMethod( CanonicalIsotopeLabelling, "for a quasigroup",
    [ IsQuasigroup ],
function( Q )
	local dg, perm;
	dg := DG4L_LatinSquare2Digraph_NC( MultiplicationTable( Q ) );
	perm := BlissCanonicalLabelling( dg, CanonicalIsotopeColoring( Q ) ); 
	return perm;
end );

###################################################################

InstallMethod( IsostrophismQuasigroups, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( Q1, Q2 )
	local iso, n;
	if Size(Q1) <> Size(Q2) then 
		return fail; 
	fi;
	n := Size(Q1);
	iso := CanonicalIsostropheLabelling(Q1) * CanonicalIsostropheLabelling(Q2)^-1;
	if not IsIsostrophismPerm( iso, n ) then 
		return fail;
	fi;
	if DG4L_IsostrophismOnLatinSquare_NC( MultiplicationTable(Q1), iso ) = MultiplicationTable(Q2) then
		return iso;
	else
		return fail;
	fi;
end );

InstallMethod( IsotopismQuasigroups, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( Q1, Q2 )
	local iso, n;
	if Size(Q1) <> Size(Q2) then 
		return fail; 
	fi;
	n := Size(Q1);
	iso := CanonicalIsotopeLabelling(Q1) * CanonicalIsotopeLabelling(Q2)^-1;
	if not IsIsotopismPerm( iso, n ) then 
		return fail;
	fi;
	if DG4L_IsostrophismOnLatinSquare_NC( MultiplicationTable(Q1), iso ) = MultiplicationTable(Q2) then
		return iso;
	else
		return fail;
	fi;
end );

InstallMethod( QuasigroupsUpToIsotopism, "for a list of quasigroups and a string",
    [ IsList, IsString ],
function( li, s )
	local upto, todo, new;
	if not ForAll( li, IsQuasigroup ) then
		Error( "the first argument must be a list of quasigroups" );
	fi;
	upto := [];
	todo := [ 1..Length(li) ];
	while todo <> [] do
		new := Filtered( todo, i -> fail<>IsotopismQuasigroups( li[todo[1]], li[i] ) );
		Add( upto, new );
		todo := Difference( todo, new );
	od;
	if s = "indices" then
		return upto;
	elif s = "sublist" then 
		return List( upto, x -> li[x[1]] );
	else	
		Error( "the 2nd argument must be either \"sublist\" or \"indices\"" );
	fi;
end );

InstallMethod( QuasigroupsUpToIsostrophism, "for a list of quasigroups and a string",
    [ IsList, IsString ],
function( li, s )
	local upto, todo, new;
	if not ForAll( li, IsQuasigroup ) then
		Error( "the first argument must be a list of quasigroups" );
	fi;
	upto := [];
	todo := [ 1..Length(li) ];
	while todo <> [] do
		new := Filtered( todo, i -> fail<>IsostrophismQuasigroups( li[todo[1]], li[i] ) );
		Add( upto, new );
		todo := Difference( todo, new );
	od;
	if s = "indices" then
		return upto;
	elif s = "sublist" then 
		return List( upto, x -> li[x[1]] );
	else	
		Error( "the 2nd argument must be either \"sublist\" or \"indices\"" );
	fi;
end );

InstallOtherMethod( QuasigroupsUpToIsotopism, "for a list of quasigroups",
    [ IsList ],
function( li )
	return QuasigroupsUpToIsotopism( li, "sublist" );
end );

InstallOtherMethod( QuasigroupsUpToIsostrophism, "for a list of quasigroups",
    [ IsList ],
function( li )
	return QuasigroupsUpToIsostrophism( li, "sublist" );
end );

###################################################################

InstallMethod( 3NetOfQuasigroup, "for a quasigroup",
    [ IsQuasigroup ],
function( Q )
local ct, n, bls;
	ct := MultiplicationTable( Q );
	n := Size( Q );
	bls := Concatenation( 
		List( [0..n-1], i -> i*n + [1..n] ),
		List( [1..n], i -> i + n*[0..n-1] ),
		List( [1..n], k -> Set( [1..n], i -> Position( ct[i], k ) + n*(i-1) ) )
	);
	return Set( bls );
end );

InstallMethod( LatinSquareDesignOfQuasigroup, "for a quasigroup",
    [ IsQuasigroup ],
function( Q )
	return DG4L_LatinSquare2LSDesign_NC( MultiplicationTable( Q ) );
end );

InstallMethod( 3NetCollineationGroup_OnPoints, "for a quasigroup",
    [ IsQuasigroup ],
function( Q )
local bls;
	bls := DG4L_LatinSquare2LSDesign_NC( MultiplicationTable( Q ) );
	return Action(AutostrophismGroup(Q),bls,OnSets);
end );

InstallMethod( 3NetCollineationGroup_OnDirections, "for a quasigroup",
    [ IsQuasigroup ],
function( Q )
	return Action(AutostrophismGroup(Q),[[1..Size(Q)],Size(Q)+[1..Size(Q)],2*Size(Q)+[1..Size(Q)]],OnSets);
end );

###################################################################

InstallMethod( SmallLoopIdentification, "for a loop",
    [ IsLoop ],
function( Q )
	if Size(Q)>6 then
		Error("small loops are listed only of order up to 6");
	fi;
	if Size(Q) in [1,2,3] then
		return [Size(Q),1];
	fi;
	if Size(Q)=4 then
		if Exponent(Q)=4 then 
			return [4,1];
		else
			return [4,2];
		fi;
	fi;
	if Size(Q)=5 then
		if IsAssociative(Q) then
			return [5,6];
		else
			return [5,First([1..5],i->fail<>IsomorphismLoops(Q,SmallLoop(5,i)))];
		fi;
	fi;
	if Size(Q)=6 then
		if IsAssociative(Q) then 
			if IsCommutative(Q) then 
				return [6,2+107];
			else
				return [6,1+107];
			fi;
		fi;
		return [6,First([1..107],i->fail<>IsomorphismLoops(Q,SmallLoop(6,i)))];
	fi;
end);


InstallMethod( DenesKeedwellIdentification, "for a loop",
    [ IsLoop ],
function(Q)
	if Size(Q)>6 then 
		Error("Denes-Keedwell classification only of order up to 6");
	fi;
	return DenesKeedwellIdentification(SmallLoopIdentification(Q));
end);

InstallOtherMethod( DenesKeedwellIdentification, "for a pair of integers",
    [ IsList ],
function(pair)
	local dk_info;
	if not (Length(pair)=2 and IsPosInt(pair[1]) and IsPosInt(pair[2])) then
		Error("argument must be a pair of positive integers");
	fi;
	if pair[1]>6 then 
		Error("Denes-Keedwell classification only of order up to 6");
	fi;
	dk_info:=[["#1.1.1.1-()"],["#2.1.1.1-()"],["#3.1.1.1-()"],["#4.1.1.1-()","#4.2.1.1-()"]];
	dk_info[5]:=[ "#5.2.1.3-()", "#5.2.1.4-()", "#5.2.1.1-()", "#5.2.1.2-()", "#5.2.1.5-()", "#5.1.1.1-()" ];
	dk_info[6]:=[ "#6.4.1.1-()", "#6.4.1.2-()", "#6.9.1.2-(1,3)", "#6.12.1.5-(1,3)", "#6.12.1.2-(1,3)", 
		"#6.12.1.3-(1,3)", "#6.12.1.1-(1,3)", "#6.9.1.1-(1,3)", "#6.5.1.3-()", "#6.5.1.2-()", "#6.5.1.3-(2,3)", "#6.10.1.1-(1,3)", 
		"#6.8.1.1-(1,3)", "#6.10.1.3-(1,3)", "#6.5.1.3-(1,3)", "#6.10.1.1-(2,3)", "#6.10.1.3-(2,3)", "#6.7.1.1-()", "#6.11.1.3-(1,3)", 
		"#6.10.1.1-()", "#6.10.1.3-()", "#6.7.1.5-(1,2,3)", "#6.5.1.1-()", "#6.10.1.4-(2,3)", "#6.10.1.4-()", "#6.10.1.2-(1,3)", 
		"#6.11.1.2-(1,3)", "#6.7.1.7-(1,2,3)", "#6.10.1.2-(2,3)", "#6.7.1.2-()", "#6.11.1.4-(1,3)", "#6.7.1.2-(1,3)", "#6.10.1.2-()", 
		"#6.11.1.1-(1,3)", "#6.7.1.2-(2,3)", "#6.8.1.2-(1,3)", "#6.8.1.1-()", "#6.11.1.3-()", "#6.7.1.5-(2,3)", "#6.11.1.1-()", 
		"#6.12.1.4-()", "#6.6.1.1-()", "#6.7.1.7-(2,3)", "#6.6.1.2-()", "#6.9.1.2-(2,3)", "#6.12.1.3-(2,3)", "#6.9.1.1-()", 
		"#6.12.1.5-()", "#6.11.1.5-(1,3)", "#6.7.1.5-()", "#6.12.1.5-(2,3)", "#6.12.1.1-(2,3)", "#6.12.1.1-()", "#6.6.1.1-(1,3)", 
		"#6.11.1.3-(2,3)", "#6.12.1.4-(1,3)", "#6.12.1.2-(2,3)", "#6.6.1.1-(2,3)", "#6.11.1.5-(2,3)", "#6.9.1.1-(2,3)", 
		"#6.12.1.4-(2,3)", "#6.8.1.1-(2,3)", "#6.10.1.4-(1,3)", "#6.7.1.7-()", "#6.11.1.2-(2,3)", "#6.11.1.4-(2,3)", "#6.8.1.2-(2,3)", 
		"#6.11.1.1-(2,3)", "#6.12.1.3-()", "#6.9.1.2-()", "#6.11.1.5-()", "#6.12.1.2-()", "#6.11.1.4-()", "#6.8.1.2-()", 
		"#6.11.1.2-()", "#6.9.1.3-(2,3)", "#6.9.1.3-(1,3)", "#6.3.1.1-()", "#6.9.1.3-()", "#6.12.1.6-(2,3)", "#6.12.1.6-(1,3)", 
		"#6.12.1.6-()", "#6.12.1.9-(1,3,2)", "#6.12.1.9-(2,3)", "#6.12.1.7-(2,3)", "#6.12.1.7-(1,3,2)", "#6.11.1.6-(1,3)", 
		"#6.11.1.6-(1,3,2)", "#6.12.1.9-(1,3)", "#6.12.1.7-()", "#6.6.1.8-(2,3)", "#6.6.1.3-(2,3)", "#6.12.1.9-()", "#6.6.1.8-()", 
		"#6.7.1.9-()", "#6.7.1.9-(2,3)", "#6.12.1.7-(1,3)", "#6.6.1.3-()", "#6.11.1.6-(1,2,3)", "#6.11.1.6-(2,3)", "#6.6.1.3-(1,3)", 
		"#6.12.1.7-(1,2)", "#6.12.1.9-(1,2,3)", "#6.11.1.6-(1,2)", "#6.11.1.6-()", "#6.12.1.7-(1,2,3)", "#6.12.1.9-(1,2)", 
		"#6.2.1.1-()", "#6.1.1.1-()" ];
	if not IsBound(dk_info[pair[1]][pair[2]]) then
		Error("wrong second argument");
	fi;
	return dk_info[pair[1]][pair[2]];
end);

###################################################################

