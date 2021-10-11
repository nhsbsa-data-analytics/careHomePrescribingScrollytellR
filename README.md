# Care home prescribing scrollytell

This R package has been developed by NHS Business Services Authority Data Analytics Learning Lab to use a combination of analytics and user research to explore prescribing in care homes in England. 

## Features

We have used the `{golem}` framework to develop this taking inspiration from:

* [Example `golem` apps](https://github.com/ThinkR-open/golem)
* [ONS Exploring local income deprivation scrollytelling article](https://www.ons.gov.uk/visualisations/dvc1371/#/E07000223)
* [`statistiekcbs/scrollytell` R package](https://github.com/statistiekcbs/scrollytell)
* [Metropolitan Council scrollytelling dashboard](https://github.com/Metropolitan-Council/service.allocation.viz)
* [Clinical Development Unit Data Science Team dashboards](https://github.com/CDU-data-science-team)

## Structure

The package is structured as follows:

```
careHomePrescribingScrollytellR
├── .github                                 # Workflows for github actions
├── R                                       # R code for the dashboard
│   ├── _disable_auotload.R                 # Golem file
│   ├── app_config.R                        # Golem file
│   ├── app_server.R                        # Server component
│   ├── app_ui.R                            # UI component
│   ├── golem_*.R                           # Golem file
│   ├── mod_*.R                             # Module 
│   ├── run_app.R                           # Golem file
│   ├── utils_helpers.R                     # Custom NHSBSA highcharter theme
│   └── utils-pipe.R                        # %>% operator
├── data-raw                                # Various scripts to produce `data` files
├── data                                    # Data for the dashboard (accessible via careHomePrescribingScrollytellR::{name})
├── dev                                     # Golem files
│   ├── 01_start.R                          # Golem file (use to set up golem framework)
│   ├── 02_dev.R                            # Golem file (use to develop package)
│   ├── 03_deploy.R                         # Golem file (use to deploy package)
│   └── run_dev.R                           # Golem file (use to test development of package)
├── inst                                    # Installed files...
│   ├── app                                 # ... for the app...
│   │   └── www                             # ... made available at runtime
│   │       ├── colours.css                 # Define colour palette of NHS identity
│   │       ├── logo.jpg                    # NHS logo
│   │       ├── mod_*.md                    # Markdown text for module
│   │       └── style.css                   # CSS to defining the styling of the dashboard
│   └── golem-config.yml                    # Golem file
├── .Rbuildignore                           # Golem file
├── .gitignore                              # Currently ignoring all `data` files
├── DESCRIPTION                             # Metadata of package
├── LICENSE                                 # Apache
├── NAMESPACE                               # Automatically generated documentation by roxygen2
├── README.md                               # Brief overview of the package
├── app.R                                   # Golem file
├── careHomePrescribingScrollytellR.Rproj   # R Project file
```
