[![Build Status](https://travis-ci.org/malcolmgreaves/rex.svg?branch=master)](https://travis-ci.org/malcolmgreaves/rex) [![Coverage Status](https://coveralls.io/repos/malcolmgreaves/rex/badge.svg)](https://coveralls.io/r/malcolmgreaves/rex)
# rex
REx: Relation Extraction. Modernized re-write of the code in the master's thesis: "Relation Extraction using Distant Supervision, SVMs, and Probabalistic First-Order Logic"

[The thesis is  here.](http://reports-archive.adm.cs.cmu.edu/anon/2014/CMU-CS-14-128.pdf)


## Setup

This project uses `sbt` for build management. If you're unfamiliar with `sbt`, see the last section for some pointers.

##### Build
To download all dependencies and compile code, run `sbt compile`.

##### Test
To run all tests, execute `sbt test`.

Moreover, to see code coverage, first run `coverage`, then `test`. The coverage report will be output as an HTML file.

##### Command Line Applications
To produce bash scripts that will execute each individual command-line application within this codebase, execute `sbt pack`.


## LICENSE
Everything within this repository is copyright (2015-) by Malcolm Greaves.

Use of this code is permitted according to the stipulations of the [Apache 2](http://www.apache.org/licenses/LICENSE-2.0.txt) license.


## How to use `sbt`
When using `sbt`, it is best to start it in the "interactive shell mode". To do this, simply execute from the command line:
```bash
$ sbt
```

After starting up (give it a few seconds), you can execute the following commands:
```
compile // compiles code
pack // creates executable scripts
test // runs tests
coverage / initializes the code-coverage system, use right before 'test'
reload // re-loads the sbt build definition, including plugin definitions
update // grabs all dependencies
```

There are a _lot_ more commands for `sbt`. And a ton of community plugins that extend `sbt`'s functionality.

##### Tips

Not necessary! Just a few suggestions...

We recommend using the following configuration for sbt:

    sbt -J-XX:MaxPermSize=768m -J-Xmx2g -J-XX:+UseConcMarkSweepGC -J-XX:+CMSClassUnloadingEnabled

This gives some more memory to `sbt`, gives it a better default GC option, and enables a better class loading & unloading module.

Also export this enviornment variable before running tests:

    export SPARK_CONF_DIR="<YOUR_PATH_TO_THIS_REPO>/src/main/resources"

It will limit the logging output.