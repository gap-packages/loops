# Version 31/12/2017
# https://en.wikipedia.org/wiki/Quasigroup
###################################################################

### Global functions for internal usage

DeclareGlobalFunction( "DG4L_LatinSquare2LSDesign_NC" );
DeclareGlobalFunction( "DG4L_LSDesign2LatinSquare_NC" );
DeclareGlobalFunction( "DG4L_LatinSquare2Digraph_NC" );
DeclareGlobalFunction( "DG4L_IsostrophismOnLatinSquare_NC" );
DeclareGlobalFunction( "DG4L_NiceColoring_NC" );

### Properties for permutations representing isostrophisms

DeclareOperation( "IsIsotopismPerm", [ IsPerm, IsPosInt ] );
DeclareOperation( "IsIsostrophismPerm", [ IsPerm, IsPosInt ] );

### Action of morphisms on quasigroups

DeclareOperation( "OnQuasigroupsByIsostrophism", [ IsQuasigroup, IsPerm ] );
DeclareOperation( "OnQuasigroupsByIsotopism", [ IsQuasigroup, IsPerm ] );
DeclareOperation( "OnQuasigroupsByIsomorphism", [ IsQuasigroup, IsPerm ] );
DeclareOperation( "OnQuasigroupsByConjugation", [ IsQuasigroup, IsPerm ] );

### Groups and canonical labellings

DeclareAttribute( "AutostrophismGroup", IsQuasigroup );
DeclareAttribute( "AutotopismGroup", IsQuasigroup );
DeclareAttribute( "CanonicalIsotopeLabelling", IsQuasigroup );
DeclareAttribute( "CanonicalIsostropheLabelling", IsQuasigroup );

DeclareCategory( "IsQuasigroupForNiceColoring", IsQuasigroup );
DeclareAttribute( "CanonicalIsotopeColoring", IsQuasigroup );
DeclareAttribute( "CanonicalIsostropheColoring", IsQuasigroup );

### Computing isotopisms and isostrophisms

DeclareOperation( "IsotopismQuasigroups", [ IsQuasigroup, IsQuasigroup ] );
DeclareOperation( "IsostrophismQuasigroups", [ IsQuasigroup, IsQuasigroup ] );

DeclareOperation( "QuasigroupsUpToIsotopism", [ IsList, IsString ] );
DeclareOperation( "QuasigroupsUpToIsostrophism", [ IsList, IsString ] );

### Collineation groups of the associated (dual) 3-nets

DeclareOperation( "3NetOfQuasigroup", [ IsQuasigroup ] );
DeclareOperation( "LatinSquareDesignOfQuasigroup", [ IsQuasigroup ] );
DeclareSynonym( "Dual3NetOfQuasigroup", LatinSquareDesignOfQuasigroup );

DeclareSynonym( "3NetCollineationGroup_OnLines", AutostrophismGroup );
DeclareSynonym( "Dual3NetCollineationGroup", AutostrophismGroup );
DeclareAttribute( "3NetCollineationGroup_OnPoints", IsQuasigroup );
DeclareAttribute( "3NetCollineationGroup_OnDirections", IsQuasigroup );

### Identification of small loops; Denes-Keedwell identification

DeclareAttribute( "SmallLoopIdentification", IsLoop );
DeclareAttribute( "DenesKeedwellIdentification", IsLoop);

