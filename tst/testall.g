#############################################################################
##
#W  testall.g   Testing LOOPS                    G. P. Nagy / P. Vojtechovsky
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

LoadPackage("loops");
TestDirectory(DirectoriesPackageLibrary("loops", "tst"), rec(exitGAP := true));
FORCE_QUIT_GAP(1);
