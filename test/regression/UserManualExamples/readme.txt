This UserManualExamples folder contains individual test cases for different
sections of the manual (e.g., TestCases/CommandReference).  The test cases
are collected and run using the TestSuite files (static "create" and dynamic
"run" folders).  It is expected that the UserManualExamples regression tests
will be filled out to include all examples in the documentation.  These files
can then be provided to users for training, while continuing to serve as test
cases.  It is of utmost importance that examples shown in documentation work
because users will attempt to copy those examples.

Each document example should be in its own folder.  For example, for the
CommandReference, a folder should be creted for each command and match the
command name.  In this folder create the Command.TSTool command file to
demonstrate the command.  Also create Results and ExpectedResults subfolders.
The example should typically write the final result to a DateValue file in the
Results folder under the test case.

Also in the command folder, create a command file named Test_Command.TSTool
(the above command file preceded by Test_).  This command file should include
commands to do the following:

1) create a log file in Results
2) Remove the Results file from the example command file (above)
3) Use RunCommands() to run the example command file.
4) Use CompareFiles() to compare the DateValue files in Results and
   ExpectedResults.  In order for this step to work, the example Results file
   must be manually copy to ExpectedResults as part of the test configuration.

The Test_*.TSTool command files can be collected by running the
UserManualExamples/TestSuite/create/Create_RunTestSuite_UserManualExamples.TSTool
command file.  The test suite can then be run using the
UserManualExamples/TestSuite/run/RunTestSuite_UserManualExamples.TSTool
command file.
