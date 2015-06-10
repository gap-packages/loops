#############################################################################
##
#W  testall.g   Testing LOOPS                    G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: testall.g, v 3.0.0 2015/06/15 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

dirs := DirectoriesPackageLibrary( "loops", "tst" );
ReadTest(  Filename( dirs, "core_methods.tst" ) );
ReadTest(  Filename( dirs, "nilpot.tst" ) );
ReadTest(  Filename( dirs, "iso.tst" ) );
ReadTest(  Filename( dirs, "lib.tst" ) );
ReadTest(  Filename( dirs, "bol.tst" ) );
