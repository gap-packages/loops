SetPackageInfo( rec(
PackageName := "loops",
Subtitle := "Computing with quasigroups and loops in GAP",
Version := "3.2.0",
Date := "02/06/2016",
ArchiveURL := "http://www.math.du.edu/loops/loops-3.2.0",
ArchiveFormats := "-win.zip .tar.gz",

Persons := [
  rec( 
    LastName      := "Nagy",
    FirstNames    := "Gabor",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "nagyg@math.u-szeged.hu",
    WWWHome       := "http://www.math.u-szeged.hu/~nagyg/",
    PostalAddress := Concatenation( [
                       "Bolyai Institute, University of Szeged\n",
                       "6725 Szeged, Aradi vertanuk tere 1\n",
                       "Hungary" ] ),
    Place         := "Szeged",
    Institution   := "University of Szeged"
  ),
  rec( 
    LastName      := "Vojtechovsky",
    FirstNames    := "Petr",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "petr@math.du.edu",
    WWWHome       := "http://www.math.du.edu/~petr/",
    PostalAddress := Concatenation( [
                       "Department of Mathematics, University of Denver\n",
                       "2280 S. Vine Street\n",
                       "Denver, CO 80208\n",
                       "USA" ] ),
    Place         := "Denver",
    Institution   := "University of Denver"
  )
],

##  Status information. Currently the following cases are recognized:
##    "accepted"      for successfully refereed packages
##    "deposited"     for packages for which the GAP developers agreed 
##                    to distribute them with the core GAP system
##    "dev"           for development versions of packages 
##    "other"         for all other packages
##
Status := "accepted",
CommunicatedBy := "Leonard Soicher (QMUL)",
AcceptDate := "05/2015",

README_URL := 
  "http://www.math.du.edu/loops/README.loops",
PackageInfoURL := 
  "http://www.math.du.edu/loops/PackageInfo.g",

AbstractHTML := Concatenation( 
"The LOOPS package provides researchers in nonassociative algebra ",
"with a computational tool that integrates standard notions ",
"of loop theory with libraries of loops and group-theoretical ",
"algorithms of GAP. The package also expands GAP toward ",
"nonassociative structures."
  ),

PackageWWWHome := "http://www.math.du.edu/loops",
               
PackageDoc := rec(
  BookName  := "loops",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0_mj.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "The LOOPS Package: Loops and quasigroups for GAP",
  Autoload  := true     # only for the documentation, TEMPORARILY TURNED OFF
),


Dependencies := rec(
  GAP := ">=4.4",
  NeededOtherPackages := [],
  SuggestedOtherPackages := [],
  ExternalConditions := []
),

AvailabilityTest := ReturnTrue,
BannerString := "This version of LOOPS is ready for GAP 4.7.\n",

Autoload := false,  # false for deposited packages
TestFile := "tst/testall.g",
Keywords := ["loop", "quasigroup", "latin square", 
    "multiplication group", "inner mapping group",
    "Moufang loop", "Bol loop", "conjugacy closed loop", "automorphic loop", "Steiner loop",
    "triality", "isomorphism of loops", "isotopism of loops"]
));
