#############################################################################
##
#W  testall.g   Testing LOOPS                    G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: testall.g, v 1.5.0 2007/04/06 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

dirs := DirectoriesPackageLibrary( "loops", "tst" );
ReadTest(  Filename( dirs, "quasigrp.tst" ) );
ReadTest(  Filename( dirs, "nilpot.tst" ) );
ReadTest(  Filename( dirs, "auto.tst" ) );
ReadTest(  Filename( dirs, "lib.tst" ) );
