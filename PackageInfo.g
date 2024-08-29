SetPackageInfo( rec(
PackageName := "loops",
Subtitle := "Computing with quasigroups and loops in GAP",
Version := "3.4.4",
Date := "29/08/2024", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

Persons := [
  rec(
    LastName      := "Nagy",
    FirstNames    := "Gábor Péter",
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
    LastName      := "Vojtěchovský",
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
  GAP := ">=4.8",
  NeededOtherPackages := [],
  SuggestedOtherPackages := [],
  ExternalConditions := []
),

AvailabilityTest := ReturnTrue,
TestFile := "tst/testall.g",
Keywords := ["loop", "quasigroup", "latin square",
    "multiplication group", "inner mapping group",
    "Moufang loop", "Bol loop", "conjugacy closed loop", "automorphic loop", "Steiner loop",
    "triality", "isomorphism of loops", "isotopism of loops"],

AutoDoc := rec(
    TitlePage := rec(
        Title := "The LOOPS Package",
        Abstract := ~.AbstractHTML,
        Copyright := """
<Index>License</Index>
&copyright; 2005-2017 Gábor P. Nagy and Petr Vojtěchovský.<P/>
The &LOOPS; package is free software;
you can redistribute it and/or modify it under the terms of the
<URL Text="GNU General Public License">https://www.fsf.org/licenses/gpl.html</URL>
as published by the Free Software Foundation; either version 2 of the License,
or (at your option) any later version.
""",
        Acknowledgements := """
We thank the following people for sending us remarks and comments, and
for suggesting new functionality of the package: Muniru Asiru, Bjoern
Assmann, Andreas Distler, Ale&#353; Dr&#225;pal, Graham Ellis, Steve
Flammia, Kenneth W. Johnson, Michael K. Kinyon, Olexandr Konovalov,
Frank L&#252;beck, Jonathan D.H. Smith, David Stanovsk&#253; and Glen
Whitney.

<P/>The library of Moufang loops of order 243 was generated from data
provided by Michael C. Slattery and Ashley L. Zenisek. The library of
right conjugacy closed loops of order less than 28 was generated from
data provided by Katharina Artic. The library of right Bruck loops of
order 27, 81 was obtained jointly with Izabella Stuhl.

<P/>Gábor P. Nagy was supported by OTKA grants F042959 and T043758, and
Petr Vojtěchovský was supported by the 2006 and 2016 University of
Denver PROF grants and the Simons Foundation Collaboration Grant 210176.
""",
    ),
),

));
