#############################################################################
##
#W  mlt_search.gd  Realizing groups as multiplication groups of loops [loops]
##
#H  @(#)$Id: mlt_search.gd, v 3.0.0 2015/06/12 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

DeclareOperation( "AllLoopTablesInGroup", [IsGroup] );
DeclareOperation( "AllProperLoopTablesInGroup", [IsGroup] );
DeclareOperation( "OneLoopTableInGroup", [IsGroup] );
DeclareOperation( "OneProperLoopTableInGroup", [IsGroup] );
DeclareOperation( "AllLoopsWithMltGroup", [IsGroup] );
DeclareOperation( "OneLoopWithMltGroup", [IsGroup] );

