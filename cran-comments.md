## R CMD check results

❯ checking installed package size ... NOTE
    installed size is  5.3Mb
    sub-directories of 1Mb or more:
      doc   4.9Mb

❯ checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ‘RGLdevice.png’

0 errors ✔ | 0 warnings ✔ | 2 notes ✖


## Revision 20230427
2PL was changed to two parameter logistic model

`D3mirt` was changed to 'D3mirt'

## Revision 20230426

A longer description of the package has been added in the description file.

Regarding examples in the function documentation, integration with mirt:mirt() takes several minutes and was therefore wrapped in \dontrun{}. 

All examples have now been changed to use \donttest{} instead of \dontrun{} and no errors or warnings are reported when performing the R CMD check.

The use of \donttest{} creates a .png file in the examples section for the plot function. This leads leads to the following note in the R CMD check: 
"Found the following files/directories:
    ‘RGLdevice.png’"
The .png file, however, does not seem to be saved in the package.


## Previous revisions

Note refers to package size (5.3mb). The package contains many examples, hence the package larger size. Examples are important because the analytical approach is less known and partly contains new components.

"+ file LICENSE"" and file "LICENSE" has been removed.
