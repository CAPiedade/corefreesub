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
Version := "0.5",
Date := "11/03/2024", # dd/mm/yyyy format
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

Keywords := [ "Faithful Transitive Permutation Representations", "Core-Free subgroups","core-free" ],

AutoDoc := rec(
  TitlePage := rec(
    Copyright := """
      &corefreesub; package is free software;
      you can redistribute it and/or modify it under the terms of the
      <URL Text="GNU General Public License">http://www.fsf.org/licenses/gpl.html</URL>
      as published by the Free Software Foundation; either version 2 of the License,
      or (at your option) any later version.
      """,
    Acknowledgements := """
      The authors wish to thank all the comments, suggestions and issue reporting from users and developers of &GAP;, both past and future.
      Both authors were partially supported by CMUP, member of LASI, which is financed by Portuguese national funds through FCT – Fundação para a Ciência e a Tecnologia, I.P., under the project with references UIDB/00144/2020 and UIDP/00144/2020. 
      """,
  ),
),

));


