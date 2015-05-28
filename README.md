[![Build Status](https://travis-ci.org/malcolmgreaves/rex.svg?branch=master)](https://travis-ci.org/malcolmgreaves/rex) [![Coverage Status](https://coveralls.io/repos/malcolmgreaves/rex/badge.svg)](https://coveralls.io/r/malcolmgreaves/rex)
# rex
REx: Relation Extraction. Modernized re-write of the code in the master's thesis: "Relation Extraction using Distant Supervision, SVMs, and Probabalistic First-Order Logic"

==========================================================================================
Using sbt for building, tests, running programs, packaging, managing dependencies etc.
==========================================================================================

These are the sbt commands used in this project:
*  **test** => runs unittests in src/test
*  **scalariformFormat** => runs automatic code formatting (all code in the master branch *must* be formatted) (test:scalariformFormat formats tests)
*  **compile** => compiles code in src/main (test:compile complies tests)
*  **pack** => packages all dependencies and the project code into a folder and creates a shell script that allows one to execute main() methods in the project. Used for installing this project.
*  **update** => downloads all dependencies
*  **reload** => When in an interactive sbt session, reload will parse and load the build.sbt file. This is very useful when updating dependencies or adding plugins. (start an interactive sessions by invoking sbt with no commands: ./sbt)
*  **gen-idea** => makes project files for Intellij IDEA 
*  **eclipse** => makes project files for Eclipse

We recommend using the following configuration for sbt:

    sbt -J-XX:MaxPermSize=768m -J-Xmx2g -J-XX:+UseConcMarkSweepGC -J-XX:+CMSClassUnloadingEnabled

Also export this enviornment variable before running tests:

    export SPARK_CONF_DIR=/Users/mgreaves/rex/src/main/resources

It will limit the logging output.

LICENSE
=======
Everything within this repository is copyright (2015-) by Malcolm Greaves.

Use of this code is permitted according to the stipulations of the [Apache 2](http://www.apache.org/licenses/LICENSE-2.0.txt) license.
