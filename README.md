## The Rock the JVM ZIO Course Repository

Powered by [Rock the JVM!](rockthejvm.com)

This repository contains the code we wrote during the Rock the JVM [ZIO course](https://rockthejvm.com/p/zio). Unless explicitly mentioned, the code in this repository is exactly what was caught on camera.

This repository uses **Scala 3**. 

### Install and setup

- install your favorite IDE - in the course I work with [IntelliJ IDEA](https://jetbrains.com/idea)
- either clone the repo or download as zip
- open with the IDE as an SBT project

You can easily change the code to Scala 2 by changing the `scalaVersion` in `build.sbt`, removing the `utils` package (native to Scala 3) and replacing all `given`s with `implicit`s. See more [here](https://blog.rockthejvm.com/givens-vs-implicits/).

### For questions or suggestions

If you have changes to suggest to this repo, either
- submit a GitHub issue
- tell me in the course Q/A forum
- submit a pull request!
