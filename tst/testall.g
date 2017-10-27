#############################################################################
##
#W  testall.g   Testing LOOPS                    G. P. Nagy / P. Vojtechovsky
##
#H  @(#)$Id: testall.g, v 3.4.0 2017/10/26 gap Exp $
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

dirs := DirectoriesPackageLibrary( "loops", "tst" );
Test(  Filename( dirs, "core_methods.tst" ), rec( compareFunction := "uptowhitespace" ) );
Test(  Filename( dirs, "nilpot.tst" ), rec( compareFunction := "uptowhitespace" ) );
Test(  Filename( dirs, "iso.tst" ), rec( compareFunction := "uptowhitespace" ) );
Test(  Filename( dirs, "lib.tst" ), rec( compareFunction := "uptowhitespace" ) );
Test(  Filename( dirs, "bol.tst" ), rec( compareFunction := "uptowhitespace" ) );
