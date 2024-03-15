[![codecov](https://codecov.io/github/CAPiedade/corefreesub/coverage.svg?branch=master&token=)](https://codecov.io/gh/CAPiedade/corefreesub)
# GAP Package corefreesub

The CoreFreeSub package is a package to compute faithful transitive permutation representations (FTPRs) of groups. Moreover, this package provides tools to determine the degrees of these FTPRs and lists core-free subgroups. Furthermore, this package allows a graph representation of the permutation representations.

For more information, please refer to the package manual, found in the webpage of the package https://capiedade.github.io/corefreesub/ .


## Dependencies
This package requires GAP version at least 4.11.

Due to its dependency on pc-groups to calculate core-free subgroups of solvable groups, this package also requires the [polycyclic](https://gap-packages.github.io/polycyclic/) GAP package.

In order to produce graph outputs as images, [graphviz](https://graphviz.org/), version at least 2.43.0, should be installed.
Moreover, to write the graphs in Tikz as a TeX file, [dot2tex](https://github.com/xyz2tex/dot2tex), version at least 2.11.3, should also be installed.

## Authors
[Claudio Alexandre Piedade](https://capiedade.github.io/) & [Manuel Delgado](https://cmup.fc.up.pt/cmup/mdelgado/)


## Citing
Please, cite this package as

[PD24]  Piedade,  C.  A. and Delgado, M., corefreesub, A GAP Package for calculating  the  core-free  subgroups  and  their  faithful  transitive permutation   representations,   Version   0.5   (2024),   GAP  package,
https://CAPiedade.github.io/corefreesub/.

You can get more info by typing Cite("corefreesub"); in the gap prompt.


## Features
The features of this package include

- function to check whether a subgroup is core-free;
- computing (conjugacy classes of) subgroups which are core-free;
- computing faithful transitive permutation representations (FTPRs) and respective degrees;
- computing minimal FTPRs and respective minimal degree;
- DOT file construction of the FTPRs
- Visualization of the DOT file and TeX file output.

There is a manual in the sub-directory doc written using the GAP package gapdoc which describes the available functions in detail. The pdf, html versions of the manual are also available there.


## Contact and Feedback
For bug reports, feature requests and suggestions, please use the issue tracker.


## License
This package is under GPL 2.0 license or later.
