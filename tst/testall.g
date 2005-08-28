#############################################################################
##
#W  testall.g   Testing LOOPS                    G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: testall.g, v 0.997 2004/10/13 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

ReadTest( "quasigrp.tst" );
ReadTest( "nilpot.tst" );
ReadTest( "auto.tst" );
ReadTest( "lib.tst" );

# WARNING: The following test takes up to 10 minutes to complete.

ReadTest( "mouflib.tst" );
