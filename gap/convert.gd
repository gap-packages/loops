############################################################################
##
#W  convert.gd              Converting between numerical bases [loops]
##  
#H  @(#)$Id: convert.gd, v 3.0.0 2015/06/02 gap Exp $
##  
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

# auxiliary
DeclareOperation( "LOOPS_DigitToChar", [ IsInt ] );
DeclareOperation( "LOOPS_CharToDigit", [ IsChar ] );
DeclareOperation( "LOOPS_EncodeCayleyTable", [ IsList ] );
DeclareOperation( "LOOPS_DecodeCayleyTable", [ IsString ] );
DeclareOperation( "LOOPS_ConvertToDecimal", [ IsString, IsInt ] );
DeclareGlobalFunction( "LOOPS_ConvertFromDecimal" ); # has variable number of arguments
DeclareGlobalFunction( "LOOPS_ConvertBase" ); # has variable number of arguments 

