SetPackageInfo( rec(
PackageName := "loops",
Subtitle := "Computing with quasigroups and loops in GAP",
Version := "2.2.0",
Date := "28/06/2012",
ArchiveURL := "http://www.math.du.edu/loops/loops-2.2.0",
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
                       "2360 S. Gaylord Street\n",
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
Status := "deposited",

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
  Archive := "",
  ArchiveURLSubset := ["doc", "htm"],
  HTMLStart := "htm/chapters.htm",
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
BannerString := "This version of LOOPS is ready for GAP 4.5.\n",

Autoload := false,  # false for deposited packages
TestFile := "tst/testall.g",
Keywords := ["loop", "quasigroup", "Moufang loop", "Bol loop", 
    "Steiner loop", "triality"]
));
