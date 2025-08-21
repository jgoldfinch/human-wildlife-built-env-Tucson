# human-wildlife-built-env-Tucson
A repository to recreate the analysis in the Waller et al. (in prep) publication examining human-wildlife interactions relative to built environment features.

## Programs needed to run:
* R

## Code files needed to run:
* final_code_08_21_2025.R

## Data files needed to run:
* project_area.shp
* tucson_water_500ft_buffer.shp
* tucson_park_500ft_buffer.shp
* tucson_roads.shp
* tucson_neighborhoods.shp
* mammal_obs_Tucson.csv
* tucson_roadkill.csv
* tucson_animal_bites.csv


## Complete the following steps in this order:

1) Open the *final_code_08_21_2025.R* file (all R code will be run from here and line numbers below refer to this file).

2) Modify the appropriate working directory (line 16) for your computer that contains the data listed under “Data files needed to run” above. This will also be where images are saved (if you choose), so make sure it is somewhere you would like to save images too.

3) Load packages needed to run the script (lines 20-29).

4) Load in spatial and interaction data – run lines 36-155.

5) Check that the data has been loaded in and explore it using the interactive map created with the mapview package. Run lines 161-168.

6) Calculate the proportion of observations within the different buffers of each built-environment feature (manuscript research question #1). Run lines 178-214 to create the data frames. 

7) Generate the same # of random points in the project area as total observations. Run lines 222-236.

8) Check that the random points have been appropriately generated and explore them using the interactive map created again with mapview. Run lines 243-251.

9) Create data frames to combine all the data for plotting. Run lines 257-341.

10) Create pie chart plots that show the proportions of observations within the different buffers of each built-environment feature (manuscript research question #1, basis for Figures 2a and 2b). Run lines 347-643. Note there is code on lines 459/460 and 642/643 to save these outputs in different figure formats if desired.

11) Create summary plots of interactions by neighborhood (manuscript research question #4, basis for Figure 1b). Run lines 655-693. Note there is code on lines 686-693 to save these outputs in different figure formats if desired. 

12) Create a scatter plot of conflict/coexistence interactions by features (manuscript research question #3, basis for Figure 3a). Run lines 702-720. Note there is code on lines 723/724 to save this output in different figure formats if desired.

13) Center and scale the counts (calculate z-scores) for each interaction type within each buffer to compare the impact of each built-environment feature on the interactions. (manuscript research question #3, basis for Figure 4b). Run lines 736-741.

