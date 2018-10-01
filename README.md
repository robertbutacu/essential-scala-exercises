# Essential Scala Exercises

by Dave Gurnell, Copyright (C) 2018 Underscore Consulting LLP, licensed Apache 2.0.

This repository contains exercises and solutions for
[Underscore's Essential Scala][course] training course.

If you want to discuss the content or exercises with the authors,
join us in our chat room on [Gitter][gitter].

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)][gitter]

## Using the Source Code

This repository contains two branches: one for `exercises` and one for `solutions`.
The directory structure is the same in each branch.

You will need to have Git and Java and an internet connection to run the exercises.
All other dependencies are either included with the repo
or downloaded on demand during compilation.

### Notes on Editors and IDEs

If you don't have a particular preference for a Scala editor or IDE,
we recommend you do the exercises for this course using
[Atom][atom] or [VSCode][vscode] and a Linux or OS X terminal.
See the instructions below to get started.

If you want to use IntelliJ IDEA,
follow the instructions for [Importing an SBT Project][intellij-setup]
in the IntelliJ online documentation.

### Getting Started on Linux or OS X

To get started:

1. To run these exercises you need a Java 8 compatible JDK on your machine.
   If you don't have this already, you can download and install one from 
   [Oracle Java SE Development Kit 8+][oraclejdk] or 
   [OpenJDK 8+][openjdk].

2. Clone this repository to a directory on your hard drive,
   e.g. `~/essential-scala-exercises`:

   ~~~
   bash$ git clone https://github.com/underscoreio/essential-scala-exercises.git
   ~~~

3. Change to the root directory in the repository:

   ~~~
   bash$ cd essential-scala-exercises
   ~~~

4. Run the `sbt.sh` script.
   You may have to wait while SBT downloads various dependencies:

   ~~~
   bash$ ./sbt.sh
   # Lots of output here...
   # The first run will take a while...

   >
   ~~~

5. Type `hello/run` at the SBT prompt.
   You may have to wait while SBT downloads various dependencies.

5. If you see the message `"Hello world!"`, you're ready to go!

*If you have any problems getting started, get in touch on [Gitter][gitter].*

### Getting Started on Windows

You will need to have installed Git and Java (we recommend Oracle's Java 7 SDK).
Complete the following steps outlined in Chapter 1 in the section entitled
"Setting up SBT for This Book":

1. To run these exercises, you need a Java 8 compatible JDK on your machine.
   If you don't have this already, you can download and install one from 
   [Oracle Java SE Development Kit 8+][oraclejdk] or 
   [OpenJDK 8+][openjdk].

2. Clone this repository to a directory on your hard drive,
   e.g. `C:\essential-scala-exercises`:

   ~~~
   C:\> git clone https://github.com/underscoreio/essential-scala-exercises.git ↩
                    C:\essential-scala-exercises
   ~~~

3. Change to the root directory in the repository:

   ~~~
   C:\> cd\essential-scala-exercises
   ~~~

4. Run the `sbt.bat` script.
   You may have to wait while SBT downloads various dependencies:

   ~~~
   C:\essential-scala-exercises> sbt
   # Lots of output here...
   # The first run will take a while...

   >
   ~~~

5. Type `hello/run` at the SBT prompt.
   You may have to wait while SBT downloads various dependencies.

6. If you see the message `"Hello world!"`, you're ready to go!

*If you have any problems getting started, get in touch on [Gitter][gitter].*


[course]: http://underscore.io/training/courses/essential-scala
[atom]: https://atom.io
[vscode]: https://code.visualstudio.com/
[intellij-idea]: https://www.jetbrains.com/idea
[intellij-setup]: https://www.jetbrains.com/help/idea/2016.1/getting-started-with-sbt.html#import_project
[gitter]: https://gitter.im/underscoreio/scala?utm_source=essential-scala-readme&utm_medium=badge&utm_campaign=essential-scala
[oraclejdk]: http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html
[openjdk]: http://openjdk.java.net/install/
