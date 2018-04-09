SetPackageInfo( rec(
PackageName := "loops",
Subtitle := "Computing with quasigroups and loops in GAP",
Version := "3.4.0dev",
Date := "27/10/2017",

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
  ),
],

Status := "accepted",
CommunicatedBy := "Leonard Soicher (QMUL)",
AcceptDate := "05/2015",

PackageWWWHome  := "https://gap-packages.github.io/loops/",
README_URL      := Concatenation( ~.PackageWWWHome, "README" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
SourceRepository := rec(
    Type := "git",
    URL := "https://github.com/gap-packages/loops",
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/loops-", ~.Version ),
ArchiveFormats := ".tar.gz",

AbstractHTML := Concatenation( 
"The LOOPS package provides researchers in nonassociative algebra ",
"with a computational tool that integrates standard notions ",
"of loop theory with libraries of loops and group-theoretical ",
"algorithms of GAP. The package also expands GAP toward ",
"nonassociative structures."
),
               
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
  GAP := ">=4.7",
  NeededOtherPackages := [],
  SuggestedOtherPackages := [],
  ExternalConditions := []
),

AvailabilityTest := ReturnTrue,
TestFile := "tst/testall.g",
Keywords := ["loop", "quasigroup", "latin square", 
    "multiplication group", "inner mapping group",
    "Moufang loop", "Bol loop", "conjugacy closed loop", "automorphic loop", "Steiner loop",
    "triality", "isomorphism of loops", "isotopism of loops"]
));
