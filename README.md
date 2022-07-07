## The Rock the JVM ZIO Course Repository

Powered by [Rock the JVM!](rockthejvm.com)

This repository contains the code we wrote during the Rock the JVM [ZIO course](https://rockthejvm.com/p/zio). Unless explicitly mentioned, the code in this repository is exactly what was caught on camera.

This repository uses **Scala 3**. 

### Install and setup

- install your favorite IDE - in the course I work with [IntelliJ IDEA](https://jetbrains.com/idea)
- either clone the repo or download as zip
- open with the IDE as an SBT project

You can easily change the code to Scala 2 by changing the `scalaVersion` in `build.sbt`, removing the `utils` package (native to Scala 3) and replacing all `given`s with `implicit`s. See more [here](https://blog.rockthejvm.com/givens-vs-implicits/).

### Getting Started

Start by cloning this repository and checkout the `start` tag:

```
git checkout start
```

The repository has tags for intermediate states of the code. This is useful to keep track of course progress, and particularly for the harder exercises where we modify the same code. To revert to that state of the code, all you have to do is `git checkout (the respective tag)`. The tags are as follows:

* `start`
* `1.1-scala-recap`
* `1.2-variance`
* `1.3-contextual-abstractions-scala-2`
* `1.4-contextual-abstractions-scala-3`
* `2.1-effects`
* `2.2-effects-exercises`
* `2.3-zio`
* `2.4-zio-exercises`
* `2.5-zio-apps`
* `2.6-zio-error-handling`
* `2.8-layers`
* `3.1-fibers`
* `3.2-fibers-exercises`
* `3.4-interruptions`
* `3.5-parallelism`
* `3.6-resources`
* `3.7-blocking`
* `3.8-mastering-interruptions`
* `3.9-asynchronous`
* `3.10-schedules`
* `4.1-ref`
* `4.2-promise`
* `4.4-mutex`
* `4.6-semaphores`
* `4.7-stm`
* `4.8-stm-data`
* `4.9-stm-coordination`
* `5.1-testing`
* `5.2-junit-tests`
* `5.3-testing-services`
* `5.4-property-based-testing`

### Seeing the complete code

Either clone this repo as it is, or do

```
git checkout master
```

### For questions or suggestions

If you have changes to suggest to this repo, either
- submit a GitHub issue
- tell me in the course Q/A forum
- submit a pull request!
