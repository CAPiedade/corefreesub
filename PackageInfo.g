#
# corefreesub: A GAP Package for calculating the core-free subgroups and their faithful transitive permutation representations
#
# This file contains package meta data. For additional information on
# the meaning and correct usage of these fields, please consult the
# manual of the "Example" package as well as the comments in its
# PackageInfo.g file.
#
SetPackageInfo( rec(

PackageName := "corefreesub",
Subtitle := "A GAP Package for calculating the core-free subgroups and their faithful transitive permutation representations",
Version := "0.2",
Date := "08/11/2022", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

Persons := [
  rec(
    FirstNames := "Claudio Alexandre",
    LastName := "Piedade",
    WWWHome := "https://www.fc.up.pt/pessoas/claudio.piedade/",
    Email := "claudio.piedade@fc.up.pt",
    IsAuthor := true,
    IsMaintainer := true,
    #PostalAddress := TODO,
    Place := "Porto, Portugal",
    Institution := "Centro de Matemática da Universidade do Porto",
  ),
  rec(
    FirstNames := "Manuel",
    LastName := "Delgado",
    WWWHome := "https://cmup.fc.up.pt/cmup/mdelgado/",
    Email := "mdelgado@fc.up.pt",
    IsAuthor := true,
    IsMaintainer := true,
    #PostalAddress := TODO,
    Place := "Porto, Portugal",
    Institution := "Centro de Matemática da Universidade do Porto",
  ),
],

SourceRepository := rec(
    Type := "git",
    URL := "https://github.com/CAPiedade/corefreesub",
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome  := "https://CAPiedade.github.io/corefreesub/",
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
README_URL      := Concatenation( ~.PackageWWWHome, "README.md" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/", ~.PackageName, "-", ~.Version ),

ArchiveFormats := ".tar.gz",

##  Status information. Currently the following cases are recognized:
##    "accepted"      for successfully refereed packages
##    "submitted"     for packages submitted for the refereeing
##    "deposited"     for packages for which the GAP developers agreed
##                    to distribute them with the core GAP system
##    "dev"           for development versions of packages
##    "other"         for all other packages
##
Status := "dev",

AbstractHTML   :=  "",

PackageDoc := rec(
  BookName  := "corefreesub",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0_mj.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "A GAP Package for calculating the core-free subgroups and their faithful transitive permutation representations",
),

Dependencies := rec(
  GAP := ">= 4.11",
  NeededOtherPackages := [ ],
  SuggestedOtherPackages := [ ],
  ExternalConditions := [ ],
),

AvailabilityTest := ReturnTrue,

TestFile := "tst/testall.g",

#Keywords := [ "TODO" ],

));


