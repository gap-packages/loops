# Version 31/12/2017
# https://en.wikipedia.org/wiki/Quasigroup
###################################################################

Q0:=RandomLoop(10);
dpg:=DirectProduct(SymmetricGroup(Size(Q0)),SymmetricGroup(Size(Q0)),SymmetricGroup(Size(Q0)));;
Q:=OnQuasigroupsByIsotopism(Q0,Random(dpg));
R:=OnQuasigroupsByIsotopism(Q0,Random(dpg));
S:=OnQuasigroupsByConjugation(R,(1,2,3));

IsotopismQuasigroups(Q,R);
IsostrophismQuasigroups(Q,Opposite(R));

IsotopismQuasigroups(Q,S);
IsostrophismQuasigroups(Q,S);

###################################################################

Q0:=IntoLoop(OneSmallGroup(Size,21,IsAbelian,false));
Q1:=AssociatedRightBruckLoop(Q0);
ct:=List(Q1,x->List(Q1,y->Position(Elements(Q1),(y/x)*y)));
Q2:=QuasigroupByCayleyTable(ct);
it:=IsotopismQuasigroups(Q1,Q2);
Q1a:=OnQuasigroupsByIsotopism(Q1,it);
IsomorphismQuasigroups(Q1a,Q2);

Size(AutostrophismGroup(Q1)); Runtime();
Size(AutotopismGroup(Q1)); Runtime(); 
KnownAttributesOfObject(Q1);

OrbitLengths(3NetCollineationGroup_OnPoints(Q1));

###################################################################

Q:=MoufangLoop(16,1);
net:=3NetOfQuasigroup(Q); 
ForAll(net,IsSet);
coll:=3NetCollineationGroup_OnPoints(Q);
Size(coll);
List(GeneratorsOfGroup(coll),g->net=OnSetsSets(net,g));

###################################################################

R:=OnQuasigroupsByIsomorphism(Q,(3,6));
IsomorphismQuasigroups(R,Q);

Q0:=RandomLoop(30);
dpg:=DirectProduct(SymmetricGroup(Size(Q0)),SymmetricGroup(Size(Q0)),SymmetricGroup(Size(Q0)));;
Q:=OnQuasigroupsByIsotopism(Q0,Random(dpg));
R:=OnQuasigroupsByIsotopism(Q0,Random(dpg));
IsotopismQuasigroups(Q,R);
Print("#1: ",StringTime(Runtime()),"\n");

###################################################################

Q:=InterestingLoop(96,1);
Size(AutostrophismGroup(Q)); StringTime(Runtime()); 
Size(AutotopismGroup(Q)); StringTime(Runtime()); 
Print("#2: ",StringTime(Runtime()),"\n");

###################################################################
###################################################################

upto_something:=function(li,something)
	local upto,new,todo;
	upto:=[];
	todo:=[1..Length(li)];
	while todo<>[] do
		new:=Filtered(todo,i->fail<>something(li[todo[1]],li[i]));
		Add(upto,new);
		todo:=Difference(todo,new);
	od;
	return upto;
end;

ls:=[];;
for i in [1..6] do Append(ls, [LeftBolLoop(8,i),RightBolLoop(8,i)]); od;
ls;
upto_something(ls,IsostrophismQuasigroups);
upto_something(ls,IsotopismQuasigroups);

QuasigroupsUpToIsotopism(ls,"indices");
QuasigroupsUpToIsostrophism(ls);

###################################################################

ls6:=List(AllSmallGroups(Size,6),IntoLoop);
for i in [1..107] do Add(ls6,SmallLoop(6,i)); od;

ist:=upto_something(ls6,IsostrophismQuasigroups);
Print("#3: ",StringTime(Runtime()),"\n");
Size(ist);

itp:=upto_something(ls6,IsotopismQuasigroups);
Print("#4: ",StringTime(Runtime()),"\n");
Size(itp);

QuasigroupsUpToIsotopism(ls6,"indices"); Size(last);
QuasigroupsUpToIsostrophism(ls6); Size(last);

###################################################################

cts:=[
[[1,2,3,4,5,6],[2,1,4,6,3,5],[3,6,2,5,4,1],[4,5,6,3,1,2],[5,4,1,2,6,3],[6,3,5,1,2,4]],  # 1
[[1,2,3,4,5,6],[2,1,5,6,3,4],[3,6,2,5,4,1],[4,5,6,3,1,2],[5,4,1,2,6,3],[6,3,4,1,2,5]],  # 2
[[1,2,3,4,5,6],[2,1,6,5,3,4],[3,4,5,2,6,1],[4,3,2,6,1,5],[5,6,4,1,2,3],[6,5,1,3,4,2]],  # 3
[[1,2,3,4,5,6],[2,1,4,6,3,5],[3,5,6,2,4,1],[4,6,5,3,1,2],[5,3,2,1,6,4],[6,4,1,5,2,3]],  # 4 (solv)
[[1,2,3,4,5,6],[2,1,6,5,3,4],[3,4,2,1,6,5],[4,6,5,2,1,3],[5,3,4,6,2,1],[6,5,1,3,4,2]],  # 5
[[1,2,3,4,5,6],[2,1,5,6,3,4],[3,6,2,5,4,1],[4,5,6,2,1,3],[5,4,1,3,6,2],[6,3,4,1,2,5]],  # 6 bad (9)
[[1,2,3,4,5,6],[2,1,6,5,3,4],[3,6,5,2,4,1],[4,5,2,6,1,3],[5,4,1,3,6,2],[6,3,4,1,2,5]],  # 7 bad (10)
[[1,2,3,4,5,6],[2,1,5,6,3,4],[3,5,6,2,4,1],[4,6,2,5,1,3],[5,3,4,1,6,2],[6,4,1,3,2,5]],  # 8 bad |Z|=2
[[1,2,3,4,5,6],[2,1,5,6,4,3],[3,4,1,5,6,2],[4,3,6,1,2,5],[5,6,2,3,1,4],[6,5,4,2,3,1]],  # 9
[[1,2,3,4,5,6],[2,6,5,3,4,1],[3,4,6,1,2,5],[4,5,2,6,1,3],[5,3,1,2,6,4],[6,1,4,5,3,2]],  # 10 univ. {3,6}-loop (solv)
[[1,2,3,4,5,6],[2,1,4,3,6,5],[3,6,5,2,1,4],[4,5,6,1,2,3],[5,4,1,6,3,2],[6,3,2,5,4,1]],  # 11 S3
[[1,2,3,4,5,6],[2,1,4,3,6,5],[3,4,5,6,1,2],[4,3,6,5,2,1],[5,6,1,2,3,4],[6,5,2,1,4,3]]   # 12 C6
];

ls:=List(cts,LoopByCayleyTable);
QuasigroupsUpToIsostrophism(ls);

List(ls,DenesKeedwellIdentification);

###################################################################

Print("# total: ",StringTime(Runtime()),"\n");

