# Flexible Self-organisation for the Cloud-Edge Continuum: a Macro-programming Approach - Experiments

In this repository, we provide the code and data to reproduce the experiments for the ACSOS 2024 paper
"Flexible Self-organisation for the Cloud-Edge Continuum: a Macro-programming Approach".

## Reproduce the entire experiment

**WARNING**: re-running the whole experiment may take a very long time on a normal computer.

In the [data](./data) folder (~37MB of data) you can find the already generated data,
ensuring a more lightweight process for charts reproduction and verification.

A good internet connection is required to download the necessary dependencies,
in a reasonable amount of time.

On a server with the following specifications, the experiment took around **27 hours** to complete:
- 96 cores (Intel(R) Xeon(R) Gold 5118 CPU @ 2.30GHz)
- 256 GB of RAM
- 1 GB of free disk space (the experiment generates around 500 MB of data)

### Reproduce with containers (recommended)

1. Install [docker](https://docs.docker.com/engine/install/) and [docker-compose](https://docs.docker.com/compose/install/linux/) (recommended version: 27.0.3)
2. Run `docker-compose up`
3. The charts will be available in the `charts` folder.

### Reproduce natively

1. Install Java (JDK 17 or later, recommended: [AdoptOpenJDK](https://adoptopenjdk.net/))
2. Install a Gradle-compatible version of Java.
  Use the [Gradle/Java compatibility matrix](https://docs.gradle.org/current/userguide/compatibility.html)
  to learn which is the compatible version range.
  The version of Gradle used in this experiment can be found in the `gradle-wrapper.properties` file
  located in the `gradle/wrapper` folder.
3. Install the version of Python indicated in `.python-version` (or use [pyenv](https://github.com/pyenv/pyenv)).
4. Launch either:
    - `./gradlew runAllBatch` on Linux, MacOS, or Windows if a bash-compatible shell is available;
    - `gradlew.bat runAllBatch` on Windows cmd or Powershell;
5. The results will be available in the `data` folder once the experiment is finished. Run:
    - `pip install --upgrade pip`
    - `pip install -r requirements.txt`
    - `python process.py`
6. The charts will be available in the `charts` folder.

## Inspect a single experiment

Follow the instructions for reproducing the entire experiment natively, but instead of running `runAllBatch`,
run `runEXPERIMENTGraphics`, replacing `EXPERIMENT` with the name of the experiment you want to run
(namely, with the name of the YAML simulation file).

If in doubt, run `./gradlew tasks` to see the list of available tasks.

To make changes to existing experiments or for exploring/reusing,
we recommend using the IntelliJ Idea IDE.
Opening the project in IntelliJ Idea will automatically import the project, download the dependencies,
and allowing a smooth development experience.

## Regenerate the charts

We keep a copy of the data in this repository
so that the charts can be regenerated without having to run the experiment again.
To regenerate the charts, run `docker compose run --no-deps charts`.
Alternatively, follow the steps or the "reproduce natively" section,
starting after the part describing how to re-launch the simulations.

The charts used in the paper are available in the root of the `charts` folder.
The `charts/rescue`, `charts/stable-gradient`, and `charts/stable-scr` folders contain the charts for the respective experiments by comparing each simulation variable.

## Artifact Customization

The artifact can be customized by changing the simulation parameters in the `.yml` files in the [yaml](./src/main/yaml) folder.  
The parameters defined in the `variables` section can be changed to modify the simulation.
For instance, the `nodeCount` parameter can be changed to simulate a different number of nodes.

In the [actions](./src/main/scala/it/unibo/alchemist/model/implementations/actions) folder,
you can find the modified ScaFi implementation supporting the model proposed in the paper.
In the [modularization](./src/main/scala/it/unibo/modularization) folder,
you can find the code used to implement the scenarios proposed in the paper.
