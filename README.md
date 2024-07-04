# Flexible Self-organisation for the Cloud-Edge Continuum: a Macro-programming Approach - Experiments

In this repository, we provide the code and data to reproduce the experiments for the ACSOS 2024 paper
"Flexible Self-organisation for the Cloud-Edge Continuum: a Macro-programming Approach".

## Reproduce the entire experiment

**WARNING**: re-running the whole experiment may take a very long time on a normal computer.

In the [data](./data) folder (~37MB of data) you can find the already generated data,
ensuring a more lightweight process for charts reproduction and verification.

A good internet connection is required to download the necessary dependencies,
in a reasonable amount of time.

### Reproduce with containers (recommended)

1. Install [docker](https://docs.docker.com/engine/install/) and [docker-compose](https://docs.docker.com/compose/install/linux/)
2. Run `docker-compose up`
3. The charts will be available in the `charts` folder.

### Reproduce natively

1. Install a Gradle-compatible version of Java.
  Use the [Gradle/Java compatibility matrix](https://docs.gradle.org/current/userguide/compatibility.html)
  to learn which is the compatible version range.
  The version of Gradle used in this experiment can be found in the `gradle-wrapper.properties` file
  located in the `gradle/wrapper` folder.
2. Install the version of Python indicated in `.python-version` (or use [pyenv](https://github.com/pyenv/pyenv)).
3. Launch either:
    - `./gradlew runAllBatch` on Linux, MacOS, or Windows if a bash-compatible shell is available;
    - `gradlew.bat runAllBatch` on Windows cmd or Powershell;
4. The results will be available in the `data` folder once the experiment is finished. Run:
    - `pip install --upgrade pip`
    - `pip install -r requirements.txt`
    - `python process.py`
5. The charts will be available in the `charts` folder.

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
