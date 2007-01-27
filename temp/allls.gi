# all loops of order n up to isomorphism

n := 6;

P := List([1..n], i->[1..n]); # partial latin square
R := [1..n]; # R[i] = elements available in row i
C := [1..n]; # C[i] = elements available in column i
LOOPS := [];

TryToAddLoop := function()
    local L, D, G, new_loop, data;
    L := LoopByCayleyTable(P);
    D := Discriminator(L);
    G := EfficientGenerators(L,D);
    new_loop := true;
    for data in LOOPS do
        if new_loop and not IsomorphismLoopsNC(data[1],data[2],data[3],L,D) = fail then 
            new_loop:=false;
            break;
        fi;
    od;
    if new_loop then # new loop
        Add(LOOPS, [L,G,D]);
    fi;
    return true;
end;

NextLS := function( r, c )
    # the entry to be filled is in row r and column c
    local x, available;
    if r>n then # P is completed
        TryToAddLoop();
        return true;
    fi;
    # trying to fill cell r, c
    available := Intersection( R[r], C[c]);
    # depth first
    for x in available do
        P[r][c]:=x;
        R[r]:=Difference(R[r],[x]);
        C[c]:=Difference(C[c],[x]);
        if c<n then
            NextLS( r, c+1 );
        else
            NextLS( r+1, 2 ); #because P is normalized
        fi;
        AddSet(R[r],x);
        AddSet(C[c],x);
    od;
    return true;
end;

AllLS := function()
    # initialization
    local c, r;
    for c in [1..n] do P[1][c]:=c; od; # standard first row
    for r in [1..n] do P[r][1]:=r; od; # standard first column
    for c in [2..n] do C[c]:=Difference([1..n],[c]); od; #available entries in column c
    for r in [2..n] do R[r]:=Difference([1..n],[r]); od; #available entries in row r
    NextLS(2,2);
    return true;
end;

# mistake in isomorphism of loops?

FindMistake := function()
    local X, Y, x, y, p, good, u, v;
    for X in LOOPS do for Y in LOOPS do
        if not X=Y then
            x := CayleyTable(X[1]); y := CayleyTable(Y[1]);
            for p in SymmetricGroup(6) do
                good := true;
                for u in [1..6] do for v in [1..6] do
                    if not y[u^p][v^p] = (x[u][v])^p then good := false; break; fi;
                od; od;
                if good then return [X,Y,p]; fi;
            od;
        fi;
    od; od;
    return fail;
end;

Text64 := function()
    local dir, name, output, iii, t;
    dir := Directory("c:/");
    name := Filename(dir,"temp_res.txt");
    output := OutputTextFile(name, true);
    for iii in [1..4262] do
        AppendTo(output,iii); AppendTo(output, " ");
        t := IsomorphismTypeOfMoufangLoop(MoufangLoop(64,iii))[1][2];
        if not t=iii then return [false, iii]; fi;
    od;
    CloseStream(output);
end;

weird := LoopByCayleyTable(
[
[1,2,3,4,5,6],
[2,3,1,5,6,4],
[3,1,4,6,2,5],
[4,6,5,3,1,2],
[5,4,6,2,3,1],
[6,5,2,1,4,3]
]);
   
LoopsToFile := function(lps, filename)
    local str, L, ct, s, i, j, output, dir, name;
    dir := Directory("c:/");
    name := Filename(dir, filename);
    output := OutputTextFile(name, true);
    for L in lps do
        ct := CayleyTable(L);
        for i in [2..n] do for j in [2..n] do
            AppendTo(output,ct[i][j]);
        od; od;
        AppendTo(output,"\n");
    od;
    CloseStream(output);
    return true;
end;

FirstDiff := function()
    local sLOOPS, diff, i, s1, s2, j;
    sLOOPS := CTToString();
    diff := 0;
    for i in [1..108] do
        s1 := sLOOPS[i];
        s2 := sLOOPS[i+1];
        j := 1;
        while s1[j]=s2[j] do j := j+1; od;
        diff := diff+j;
    od;
    return diff/108;
end;
        
