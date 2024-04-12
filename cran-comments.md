
##This is a re-submission
*version 1.0.0 of the avotrex package

*We have put single quotes around the two database names in the 
description, and added a URL to the one. The other is in review and
we have now made this clear.

*We have added add \value to the .Rd files of all exported functions.

*Both instances of print and cat have been changed to message().

*It is necessary for us to change the par() within our plot function, but
we have now ensured the settings are reset on exit.

*We have removed the LICENSE file and its reference in the Description.

*The doSNOW package is required to output a progress bar from the foreach loop through .options.snow argument. This is not possible in the doParallel package. 

*Updated the html to include slashes ("https://birdtree.org") within the DESCRIPTION.

##Test environments
*local Windows 10 install, R 4.3.2
*Ubuntu 20.04.6 LTS (64-bit), R 4.3.3 (via Circle CI)
 
##R CMD check results
0 errors | 0 warnings | 0 notes

##dependencies
no dependencies
