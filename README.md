# Planetary Health Index

Planetary Health Index (PHI) describes the state of a region at a given time using three sets of features: biosphere, atmosphere, and sociosphere.
Canonical Correlation Analysis (CCA) is used to describe the relationship between a given pair of those feature sets.

## Get Started

This dashboard is deployed at [https://phi.danlooo.de/](https://phi.danlooo.de/).
To run it locally, clone this repository and run:

```bash
git clone https://github.com/danlooo/planetary-health-index.git
cd planetary-health-index
docker build -t danlooo/planetary-health-index .
docker run -p 80:80 danlooo/planetary-health-index
```

The local dashboard will be available at [http://localhost/](http://localhost/).

## Development

This app is a Docker container with an R targets workflow that is triggered on `docker build`.
Targets were used in the dashboard written in R shiny.
The container provides the environment for targets, shiny, and VSCode devconatiner.

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