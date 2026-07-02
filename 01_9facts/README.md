# IDAC 9 Facts for Children on the move

Code to produce summarized data, charts and maps for the [IDAC 9 facts](https://data.unicef.org/resources/9-facts-about-children-on-the-move/).

Maps are also used in <https://data.unicef.org/topic/child-migration-and-displacement/displacement/>

## How to update 9 FACTS

### 1. Update source data

Update source files for UN migrant stock, UNHCR refugees, UNRWA refugees, IDMC IDPS and Asylum-seekers.

See [*DW_DemogMigration/03_raw_to_RData*](https://github.com/unicef-drp/DW-DemogMigration/tree/main/03_raw_to_RData)*.*

### 2. Update scripts

-   *main.R*

-   *9facts_charts.R*

-   *9facts_maps.R*

### 3. Run main file

### 4. Route map (Fact 9)

-   The route map in fact 9 is in inDesign files. See *Migration and Displacement/AllianceIDAC/IDAC_9_FACTS/Version_2022_10/Fact 9 InDesign files*

## Updates

2026.07.02

-   Start of 2026 update: rolled data year filters/ranges and raw data source paths forward to 2025/2026.

-   Merged *profile.R* into *main.R* and dropped the multi-user path branching (single user of these scripts now).

2025.10.07

-   Moving forward with 2025 version

-   Start using data directly from DW

2025.06.06

-   Start of 2025 update

-   Move from .Rmd to .R files.

-   Move from independent scripts to *profile.R* and *main.R*.

2024.06.18

-   Start of 2024 update

2024.01.25

-   Input_misc files uploaded to repository

2023.11.16

-   Added comments clarifying the calculations done for Fact 8.

-   Fact 8 now runs OK. The problem was that we weren't loading 2020 data. This code didn't need to run since we weren't updating the Fact this year.

2023.11.08

-   Added script to create Fact 3 map on UNPD migration stock to *9facts_maps.R*.

-   Several options for new fact 7 chart. Previous version was removed because of incorrect analysis. Some parts of the script may not run well since it is still in development.

2023.10.27

-   Added *presave_sp_rds_object.R*: script to convert shapefiles to .rds files for faster loading.

2023.10.25

-   IDP maps (facts 5 and 6) code moved to *9facts_maps.R*.

-   IDP maps (facts 5 and 6) in line with UNICEF Publications Toolkit September 2017, map guidelines (pages 48-52).

-   Added *pies_overplot.R*: function to create pie charts and add them to coordinates. This function is modified from `caroline::overplot`.

-   Descriptive text removed. The latest text can be found in *Migration and Displacement/AllianceIDAC/IDAC_9_FACTS/Version_2023_10/9Facts_update_Sep2023.docx*.

2023.09.04

-   First commit: using same code from September 2023.
-   Original 9 facts file: *Migration and Displacement/AllianceIDAC/IDAC_9_FACTS/9Facts_update.R*
-   Original IDP map: *Migration and Displacement/data.unicef.org/05_Update_Jun2023/make_idp_map.R*
