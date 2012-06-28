Notes on how to install and use R with TSTool.

First, install R from:  http://www.r-project.org/

To install rJava (which includes JRI components), run R and then do:

install.packages('rJava',,'http://www.rforge.net/')

R can be run on the command line using "R" (if in the path) or run R from the start menu.

To install the rJava integration packages, start R and enter the following command:

===
> install.packages('rJava',,'http://www.rforge.net/')
trying URL 'http://www.rforge.net/bin/windows/contrib/2.15/rJava_0.9-4.zip'
Content type 'application/zip' length 747815 bytes (730 Kb)
opened URL
downloaded 730 Kb


The downloaded binary packages are in
        C:\Documents and Settings\sam\Local Settings\Temp\Rtmp6TAszv\downloaded_packages
===

Also tried (note this gets an old version!):

===
> install.packages("rJava")
--- Please select a CRAN mirror for use in this session ---
trying URL 'http://rweb.quant.ku.edu/cran/bin/windows/contrib/2.15/rJava_0.9-3.zip'
Content type 'application/zip' length 746108 bytes (728 Kb)
opened URL
downloaded 728 Kb

package rJava successfully unpacked and MD5 sums checked

The downloaded binary packages are in
        C:\Documents and Settings\sam\Local Settings\Temp\Rtmp6TAszv\downloaded_packages
===

Both versions are in the temporary folder as *.zip files.  So, unzip the one that is desired.

To be continued... need to add RunR() command in TSTool to get basic configuration working.

===

Issues:

1) See http://www.rforge.net/rJava/index.html - on UNIX have to configure R for java support
2) See http://www.rforge.net/rJava/index.html - "on Windows Java is detected at run-time from the registry"
   This could be an issue since TSTool uses its own JRE and not the one installed on the system; however
   it appears that this is how rJava calls Java from R (and TSTool uses rJava in the other direction).
