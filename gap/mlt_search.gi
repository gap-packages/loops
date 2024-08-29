#############################################################################
##
#W  mlt_search.gi  Realizing groups as multiplication groups of loops [loops]
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

##########################################################################
# 
# The following commands look for loops such that the 
# multiplication group is contained in a given group <G>.
#
# The resulting loops are given by their right sections.
# 
# One can speed up the search by setting the argument <depth> 
# higher; the price is much higher memory consumption. 
# 
# <depth> is optimally chosen if in the permutation group <G>
# there are "not many" permutations fixing <depth> elements. 
# 
# You can omit this argument, or set depth=2 with no harm. 
# 
# The parameter <infolevel> determines the amount of information
# you get during the search. 
# 
# With infolevel=1, you get the information on timing and hits.
# With infolevel=2, the results are printed, as well. 
# 
##########################################################################
#
# AllLoopTablesInGroup(G[, depth[, infolevel]] )
# 	returns all loop tables <T> such that Mlt(T)<=G
# 
# AllProperLoopTablesInGroup(G[, depth[, infolevel]] )
# 	returns all non-associative loop tables <T> such that Mlt(T)<=G
# 
# OneLoopTableInGroup(G[, depth[, infolevel]] )
# 	returns one loop table <T> such that Mlt(T)<=G
# 
# OneProperLoopTableInGroup(G[, depth[, infolevel]] )
# 	returns one non-associative loop table <T> such that Mlt(T)<=G
# 
# AllLoopsWithMltGroup(G[, depth[, infolevel]] )
# 	returns all loop tables <T> such that Mlt(T)=G
# 
# OneLoopWithMltGroup(G[, depth[, infolevel]] )
# 	returns one loop table <T> such that Mlt(T)=G
# 
##########################################################################

# Debugging
LOOPS_SearchRuntime:=function(tstart)
	return Concatenation("[ ", StringTime(Runtime()-tstart)," ]");
end;

LOOPS_SearchInfo:=NewInfoClass("InfoCayleySearch");
SetInfoLevel(LOOPS_SearchInfo,1);


# LOOPS_TableSearchNC(G, depth, infolevel, task)
# 	restiction in ["", "all", "all proper", "one", "one proper", "all exact", "one exact"]
# 	if task="" then task:="all";

# auxiliary function
LOOPS_TableSearchNC:=function(g, ldepth, infolevel, task)
	local 
	# task variables
	only_one, only_proper, only_exact, takeit,
	# local external variables
	results, V, hash, row, pi, col_chunks, degree, level, depth,
	# local scanner functions
	move_right, next_node, possible_next_rows,
	# main search loop variables
	fpf_classes, reps, x0, x1, k, i, j, ct, ls, rs, 
	# debugging
	old_infolevel,time_start;

	# setting the local external variables
	results:=[];
	V:=[];
	hash:=[];
	row:=[];
	pi:=[];
	col_chunks:=[];
	degree:=0;
	level:=0;
	depth:=0;

	# functions
	move_right:=function()
		local i;
		if level=0 then 
			return false;
		fi;
		if pi[level]<>[] then 
			row[level]:=Remove(pi[level],1);
			for i in [1..degree] do
				col_chunks[i][level]:=i^V[row[level]];
			od;
			return true;
		else
			Unbind(row[level]);
			Unbind(pi[level]);
			for i in [1..degree] do
				Unbind(col_chunks[i][level]);
			od;
			level:=level-1;
			return move_right();
		fi;
	end;
	
	# next_node()
	# we call it when 
	# a) row[level+1] and pi[level+1] are not defined ---> MOVE RIGHT
	# b) row[level+1] is not defined and pi[level+1] is defined
	#    and non empty ---> MOVE DOWN
	next_node:=function()
		local i;
		if IsBound(pi[level+1]) 
			and (not IsBound(row[level+1])) 
			and (pi[level+1]<>[])
		then 
			level:=level+1;
			row[level]:=Remove(pi[level],1);
			for i in [1..degree] do
				col_chunks[i][level]:=i^V[row[level]];
			od;
			return true;
		elif (not IsBound(pi[level+1])) and (not IsBound(row[level+1])) then
			return move_right();
		else 
			return fail;
		fi;
	end;
	
	possible_next_rows:=function()
		local np,vp,dp;
		dp:=Minimum(level, depth);
		np:=List([2..degree],i->Sum([1..dp],j->(col_chunks[i][j]-1)*degree^(dp-j))+1);
		if ForAny(np,i->not(IsBound(hash[dp][i]))) then 
			pi[level+1]:=[];
			return false;
		fi;
		np:=List(np,i->[hash[dp][i][1]..hash[dp][i][1]+hash[dp][i][2]]);
		np:=List(np,x->Set(V{x},y->(level+1)^y));
		vp:=List(Cartesian(np{[1..depth-1]}),x->Concatenation([level+1],x));
		vp:=List(vp,x->Sum([1..depth],j->(x[j]-1)*degree^(depth-j))+1);
		vp:=Filtered(vp,i->IsBound(hash[depth][i]));
		vp:=List(vp,i->[hash[depth][i][1]..hash[depth][i][1]+hash[depth][i][2]]);
		vp:=Filtered(Concatenation(vp),x->
			ForAll([2..degree],i->(i^V[x] in np[i-1]) and not(i^V[x] in col_chunks[i]))
		);
		pi[level+1]:=vp;
		return vp<>[];
	end;

	# main search part
	# printing the task ["", "all", "all proper", "one", "one proper", "all exact", "one exact"]
	
	old_infolevel:=InfoLevel(LOOPS_SearchInfo);
	SetInfoLevel(LOOPS_SearchInfo,infolevel);

	if task="" then task:="all"; fi;
	if task="all" then 
		only_one:=false;
		only_proper:=false;
		only_exact:=false;
		Info(LOOPS_SearchInfo, 1, "### Search for all loops in the given group ###");
	elif task="all proper" then 
		only_one:=false;
		only_proper:=true;
		only_exact:=false;
		Info(LOOPS_SearchInfo, 1, "### Search for all nonassociative loops in the given group ###");
	elif task="one" then 
		only_one:=true;
		only_proper:=false;
		only_exact:=false;
		Info(LOOPS_SearchInfo, 1, "### Search for one loop in the given group ###");
	elif task="one proper" then 
		only_one:=true;
		only_proper:=true;
		only_exact:=false;
		Info(LOOPS_SearchInfo, 1, "### Search for one nonassociative loops loops in the given group ###");
	elif task="all exact" then 
		only_one:=false;
		only_proper:=false;
		only_exact:=true;
		Info(LOOPS_SearchInfo, 1, "### Search for all loops with given multiplication group ###");
	elif task="one exact" then 
		only_one:=true;
		only_proper:=false;
		only_exact:=true;
		Info(LOOPS_SearchInfo, 1, "### Search for one loop with given multiplication group ###");
	else 
		SetInfoLevel(LOOPS_SearchInfo,old_infolevel);
		return fail; 
	fi;

	time_start:=Runtime();
	degree:=NrMovedPoints(g);
	depth:=ldepth;
	Info(LOOPS_SearchInfo, 1, "# Size of the input group: ", Size(g));
	Info(LOOPS_SearchInfo, 1, "# Degree of the permutation group: ", degree);

	# analysis of the fixed point free elements
	fpf_classes:=Filtered(ConjugacyClasses(g),x->NrMovedPoints(Representative(x))=degree);
	if Length(fpf_classes)=1 then 
		V:=Elements(fpf_classes[1]);
	else
		V:=Union(fpf_classes);;
	fi;
	MakeImmutable(V);
	reps:=List(fpf_classes,x->Minimum(Elements(x)));
	reps:=Set(reps,x->Position(V,x));
	Info(LOOPS_SearchInfo, 1, "# ", LOOPS_SearchRuntime(time_start), " Search started."); 

	Info(LOOPS_SearchInfo, 1, "# Collected the fixed point free elements." );
	Info(LOOPS_SearchInfo, 1, "# Number of conjugacy classes = ", Size(reps), "." );
	Info(LOOPS_SearchInfo, 1, "# Number of fixed point free elements = ", Size(V), "." );

	# hash tables
	hash:=List([1..depth],i->[]);
	x0:=0*[1..depth];
	k:=0*[1..depth];
	for j in [1..Length(V)] do
		x1:=List([1..depth],i->i^V[j]);
		for i in [1..depth] do
			if x0{[1..i]}<>x1{[1..i]} then 
				k[i]:=Sum([1..i],t->(x1[t]-1)*degree^(i-t))+1;
				hash[i][k[i]]:=[j,0];
			else
				hash[i][k[i]][2]:=hash[i][k[i]][2]+1;
			fi;
		od;
		x0:=ShallowCopy(x1);
	od;
	MakeImmutable(hash);
	Info(LOOPS_SearchInfo, 1, "# ", LOOPS_SearchRuntime(time_start), " Hash table of depth ", depth, " created." );

	# initialization
	row:=['*'];
	pi:=[[],ShallowCopy(reps)];
	col_chunks:=List([1..degree],i->[i]);
	level:=1;
	results:=[];

	while next_node() do
		if level=2 then 
			Info(LOOPS_SearchInfo, 1, "# ", LOOPS_SearchRuntime(time_start), 
				" We have ", Length(pi[2])+1, " more step(s)." ); 
		fi;
		if level=degree then 
			ct:=List(V{row{[2..degree]}},ListPerm);
			ct:=Concatenation([[1..degree]],ct);
			rs:=Immutable(List(ct,PermList));
			ls:=Immutable(List(TransposedMat(ct),PermList));
			if only_exact then 
				takeit:=(Group(Union(ls,rs))=g);
			elif only_proper then
				takeit:=(ForAll(ls,y->y in g) and 
						not ForAll(rs,x->ForAll(x*rs,y->y in rs)));
			else 
				takeit:=ForAll(ls,y->y in g);
			fi;
			if takeit then 
				Add(results, ShallowCopy(row));
				Info(LOOPS_SearchInfo, 2, "##############################");
				Info(LOOPS_SearchInfo, 1, "# ", LOOPS_SearchRuntime(time_start), 
					" Hit number ", Length(results));
				Info(LOOPS_SearchInfo, 2, ct);
				if only_one then break; fi;
			fi;
		else
			possible_next_rows();
			if pi[level+1]=[] then Unbind(pi[level+1]); fi;
		fi;
	od;

	Info(LOOPS_SearchInfo, 1, "##############################");
	Info(LOOPS_SearchInfo, 1, "# ", LOOPS_SearchRuntime(time_start), " Finished. ", Length(results), " loops found." );
	
	SetInfoLevel(LOOPS_SearchInfo,old_infolevel);
	return List(results,x->Concatenation([()],V{x{[2..degree]}}));
end;

# auxiliary function
LOOPS_SearchInputCheck:=function( G, depth, infolevel, task )
	local degree;
	if not IsPermGroup(G) then 
		Info( InfoWarning, 1, "<G> must be a permutation group" );
		return false;
	fi;
	degree:=NrMovedPoints(G);
	if MovedPoints(G)<>[1..degree] then 
		Info( InfoWarning, 1, "<G> must act transitively on [1..degree]" );
		return false;
	fi;
	if depth=fail then depth:=LogInt(Size(G),degree)-1; fi;
    if not IsInt(depth) or depth<=0 then 
		Info( InfoWarning, 1, "<depth> must be a positive integer" ); 
		return false;
	fi; 
	if infolevel=fail then infolevel:=1; fi;
	if not IsInt(infolevel) or infolevel<0 then 
		Info( InfoWarning, 1, "<infolevel> must be a positive integer" ); 
		return false;
	fi; 
	if not task in ["", "all", "all proper", "one", "one proper", "all exact", "one exact"] then
		Info( InfoWarning, 1, "<task> must be one of the following:");
		Info( InfoWarning, 1, "\t\"\", \"all\", \"all proper\", \"one\", ", 
			"\"one proper\", \"all exact\", \"one exact\"." ); 
		return false;
	fi;
	return [G,depth,infolevel,task];
end;

#############################################################################
##
#O  AllLoopTablesInGroup(G[, depth[, infolevel]] )
##
## 	returns all loop tables <T> such that Mlt(T)<=G.

InstallMethod( AllLoopTablesInGroup, "for a group",
    [ IsGroup ],
function( G )
	local depth, infolevel, task, a;
	task:="all";
	depth:=fail;
	infolevel:=fail;
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( AllLoopTablesInGroup, "for a group and integer",
    [ IsGroup, IsInt ],
function( G, depth )
	local infolevel, task, a;
	task:="all";
	infolevel:=fail;
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( AllLoopTablesInGroup, "for a group and two integers",
    [ IsGroup, IsInt, IsInt ],
function( G, depth, infolevel )
	local task, a;
	task:="all";
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

#############################################################################
##
#O  AllProperLoopTablesInGroup(G[, depth[, infolevel]] )
##
## 	returns all non-associative loop tables <T> such that Mlt(T)<=G.

InstallMethod( AllProperLoopTablesInGroup, "for a group",
    [ IsGroup ],
function( G )
	local depth, infolevel, task, a;
	task:="all proper";
	depth:=fail;
	infolevel:=fail;
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( AllProperLoopTablesInGroup, "for a group and integer",
    [ IsGroup, IsInt ],
function( G, depth )
	local infolevel, task, a;
	task:="all proper";
	infolevel:=fail;
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( AllProperLoopTablesInGroup, "for a group and two integers",
    [ IsGroup, IsInt, IsInt ],
function( G, depth, infolevel )
	local task, a;
	task:="all proper";
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

#############################################################################
##
#O  OneLoopTableInGroup(G[, depth[, infolevel]] )
##
## 	returns one loop table <T> such that Mlt(T)<=G.

InstallMethod( OneLoopTableInGroup, "for a group",
    [ IsGroup ],
function( G )
	local depth, infolevel, task, a;
	task:="one";
	depth:=fail;
	infolevel:=fail;
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( OneLoopTableInGroup, "for a group and integer",
    [ IsGroup, IsInt ],
function( G, depth )
	local infolevel, task, a;
	task:="one";
	infolevel:=fail;
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( OneLoopTableInGroup, "for a group and two integers",
    [ IsGroup, IsInt, IsInt ],
function( G, depth, infolevel )
	local task, a;
	task:="one";
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

#############################################################################
##
#O  OneProperLoopTableInGroup(G[, depth[, infolevel]] )
##
## 	returns one non-associative loop table <T> such that Mlt(T)<=G.

InstallMethod( OneProperLoopTableInGroup, "for a group",
    [ IsGroup ],
function( G )
	local depth, infolevel, task, a;
	task:="one proper";
	depth:=fail;
	infolevel:=fail;
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( OneProperLoopTableInGroup, "for a group and integer",
    [ IsGroup, IsInt ],
function( G, depth )
	local infolevel, task, a;
	task:="one proper";
	infolevel:=fail;
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( OneProperLoopTableInGroup, "for a group and two integers",
    [ IsGroup, IsInt, IsInt ],
function( G, depth, infolevel )
	local task, a;
	task:="one proper";
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

#############################################################################
##
#O  AllLoopsWithMltGroup(G[, depth[, infolevel]] )
##
## 	returns all loop tables <T> such that Mlt(T)=G.

InstallMethod( AllLoopsWithMltGroup, "for a group",
    [ IsGroup ],
function( G )
	local depth, infolevel, task, a;
	task:="all exact";
	depth:=fail;
	infolevel:=fail;
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( AllLoopsWithMltGroup, "for a group and integer",
    [ IsGroup, IsInt ],
function( G, depth )
	local infolevel, task, a;
	task:="all exact";
	infolevel:=fail;
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( AllLoopsWithMltGroup, "for a group and two integers",
    [ IsGroup, IsInt, IsInt ],
function( G, depth, infolevel )
	local task, a;
	task:="all exact";
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

#############################################################################
##
#O  OneLoopWithMltGroup(G[, depth[, infolevel]] )
##
## 	returns one loop table <T> such that Mlt(T)=G.

InstallMethod( OneLoopWithMltGroup, "for a group",
    [ IsGroup ],
function( G )
	local depth, infolevel, task, a;
	task:="one exact";
	depth:=fail;
	infolevel:=fail;
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( OneLoopWithMltGroup, "for a group and integer",
    [ IsGroup, IsInt ],
function( G, depth )
	local infolevel, task, a;
	task:="one exact";
	infolevel:=fail;
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( OneLoopWithMltGroup, "for a group and two integers",
    [ IsGroup, IsInt, IsInt ],
function( G, depth, infolevel )
	local task, a;
	task:="one exact";
	a:=LOOPS_SearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return LOOPS_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

