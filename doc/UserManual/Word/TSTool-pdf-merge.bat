rem Troubleshoot PDF merge process for TSTool build
rem Code the full command line below

rem try to use the new PDF combine tool
rem "C:\Program Files (x86)\PDF Combine\PDFCombine.exe" -list ..\merge_PDF\UserManual_CDSS_Vol1_Manual_fileList.txt C:\DevRiv\TSTool_SourceBuild\TSTool\doc\UserManual\dist_CDSS2

@echo off
rem below this is old PDF merge

set rti_build_home=C:\DevRiv\TSTool_SourceBuild\rtibuild

rem Does not work...
rem %rti_build_home%\lib\pdfmerge\PMCMD.exe C:\DevRiv\TSTool_SourceBuild\TSTool\doc\UserManual\dist_CDSS\TSTool-Vol1-UserManual.pdf -FC:\DevRiv\TSTool_SourceBuild\TSTool\doc\UserManual\merge_PDF\UserManual_CDSS_Vol1_Manual_fileList.txt -B -NSTYLE=[0] -TLEVEL=1,LEVELSIZE1=10,LEVELCOLOR1=993333,TEMPLATE=%rti_build_home%/lib/pdfmerge/Table_of_Contents.pdf,TOP=2,PAGE=3 -VOPENSHOW=bookmark

rem Does not work...
rem %rti_build_home%\lib\pdfmerge\PMCMD.exe C:\DevRiv\TSTool_SourceBuild\TSTool\doc\UserManual\dist_CDSS\TSTool-Vol1-UserManual.pdf -FC:\DevRiv\TSTool_SourceBuild\TSTool\doc\UserManual\merge_PDF\UserManual_CDSS_Vol1_Manual_fileList.txt -B -VOPENSHOW=bookmark

rem Works...
rem %rti_build_home%\lib\pdfmerge\PMCMD.exe C:\DevRiv\TSTool_SourceBuild\TSTool\doc\UserManual\dist_CDSS\TSTool-Vol1-UserManual.pdf -FC:\DevRiv\TSTool_SourceBuild\TSTool\doc\UserManual\merge_PDF\UserManual_CDSS_Vol1_Manual_fileList.txt

rem %rti_build_home%\lib\pdfmerge\PMCMD.exe
