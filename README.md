# JIAF Simulation Data Preparation

JIAF simulation data to support the testing of JIAF 2.0.

## Data

Data used in the repository is shared privately by the JIAF IM team through Sharepoint/OneDrive. The filepaths in relevant data files are all relative to the top level folder 
`JIAF 2.0 development/09 - JIAF 2.0 Simulation/01 - Data for simulation`. The location of that folder on any machine should be stored as an environment variable `JIAF_SIMU_DATA_DIR`.

This should be set in your `.Renviron` file, either project-level or global. You can easily access the file with `usethis::edit_r_environ()` and adding the line:

```
JIAF_SIMU_DATA_DIR="your/file/path/here"
```
