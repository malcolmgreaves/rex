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

    sbt -J-Xmx2g -J-XX:+UseConcMarkSweepGC -J-XX:+CMSClassUnloadingEnabled

LICENSE
=======
Everything within this repository is copyright (b2015) by Malcolm Greaves.
