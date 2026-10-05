# Planet Health Index

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23158304.svg)](https://doi.org/10.5281/zenodo.23158304)

Planet Health Index (PHI) describes the state of a region at a given time using three sets of features: biosphere, atmosphere, and sociosphere.
Canonical Correlation Analysis (CCA) is used to describe the relationship between a given pair of those feature sets.

## Get Started

This dashboard is deployed at [https://phi.danlooo.de/](https://phi.danlooo.de/).
To run it locally, clone this repository and run:

```bash
git clone https://github.com/danlooo/planetary-health-index.git
cd planetary-health-index
git lfs pull
docker compose up --build
```

The local dashboard will be available at [http://localhost/](http://localhost/).

## Development

This app is a Docker container with an R targets workflow that is triggered on `docker build`.
Targets were used in the dashboard written in R shiny.
The container provides the environment for targets, shiny, and VSCode devconatiner.

## Releases

Every published GitHub release is archived on Zenodo and receives a DOI.
The workflow in `.github/workflows/zenodo-release.yml` runs on the `published` release event.
It packs the tagged source tree into a tarball, uploads it to Zenodo, and publishes it as a new version of the existing record.
Deposition metadata comes from `.zenodo.json`.

Set up the repository once:

1. Create a Zenodo personal access token with the `deposit:write` and `deposit:actions` scopes.
2. Add it as the repository secret `ZENODO_ACCESS_TOKEN`.
3. For the first release, leave the repository variable `ZENODO_CONCEPT_RECID` empty. The workflow prints the concept record ID when it finishes.
4. Store that ID in the repository variable `ZENODO_CONCEPT_RECID`. Later releases then become new versions of the same record.

To test against the Zenodo sandbox, set the repository variable `ZENODO_BASE_URL` to `https://sandbox.zenodo.org` and use a sandbox token.

Git LFS files stay pointer files in the archive.
The deposit therefore contains the source code and the tracked metadata, not the large data files.

## Funding

<p>
<a href = "https://earthmonitor.org/">
<img src="https://earthmonitor.org/wp-content/uploads/2022/04/european-union-155207_640-300x200.png" align="left" height="50" />
</a>

<a href = "https://earthmonitor.org/">
<img src="https://earthmonitor.org/wp-content/uploads/2022/04/OEM_Logo_Horizontal_Dark_Transparent_Background_205x38.png" align="left" height="50" />
</a>
</p>

This project has received funding from the [Open-Earth-Monitor Cyberinfrastructure](https://earthmonitor.org/) project that is part of the European Union's Horizon Europe research and innovation program under grant [101059548](https://cordis.europa.eu/project/id/101059548).
This project is also a collaboration with the European Central Bank.
