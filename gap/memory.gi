#############################################################################
##
#W  memory.gi                                       Memory management [loops]
##  
##  
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),  
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##
#F  LOOPS_FreeMemory( )
##
##  Frees memory by unbinding some global variables, mostly those
##  declared during activation of libraries.
##  Returns the amount of freed memory in kbytes.

InstallGlobalFunction( LOOPS_FreeMemory, function( )
    # RCC loops
    LOOPS_rcc_transitive_groups := [];
    LOOPS_rcc_sections :=  List( [1..Length(LOOPS_rcc_data[1])], i-> [] );
    LOOPS_rcc_conjugacy_classes := [ [], [] ];
    # cc loops
    LOOPS_cc_used_factors := [];
    LOOPS_cc_cocycles := [];
    LOOPS_cc_bases := [];
    LOOPS_cc_coordinates := [];
    # right Bruck loops
    LOOPS_right_bruck_cocycles := [];
    LOOPS_right_bruck_coordinates := [];
    GASMAN("collect");
    return GasmanStatistics().full.deadkb;
end);


