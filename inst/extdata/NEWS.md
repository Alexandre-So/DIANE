## \# Version changes in DIANE

### 1.0.1 : DIANE is released

Version at the moment of the official BMC publication (May 26, 2021).

### 1.0.2 : Minor bug fixes and improvements.

Improvements :

-   Citation was added
-   Gene IDs can now have blank space round them in the exploration of specific genes profiles
-   MDS was removed
-   PCA can now handle expression data with 4 or less experimental conditions
-   This document was added to the interface in a new tab

Bug fixes :

-   Custom annotation/gene information file now allows duplicated genes
-   Custom annotation/gene information now can only have columns named label and/or description
-   DEA csv download does not require annotations any more

### 1.0.3 : Minor bug fixes and improvements.

-   Fixed error in DAINE's programming interface (wrong value of the defaut "conditions"" argument in normalize function) 22/11/2021
-   Used namesapce operator in data import module to speed up loading

### 1.0.4 : Minor changes, mainly aesthetic, with no impact one DIANE's main functions and backend. Changes are from merging pull [request #36](https://github.com/OceaneCsn/DIANE/pull/36) from Alexandre-So.

-   Some code based on bootstrap grid system has been rewrite. This avoid the grey (anthracite) background to show up at the bottom of some pages, and improve shinydashboardPlus::box alignment between different dashboard tabs. This also increases a little bit the width of most of the shinydashboardPlus::box.

-   Numbers in the DEA table show less digits when displayed in DIANE (up to 10-3). The downloaded table is not affected.

-   **When a row is clicked in the DEA table, display a popup with RNAseq count associated with the related gene.** (Very cool feature, worth the highlight)

-   The little loading square position is now based on user screen resolution. The position can still be improved. Used CSS code is in app_ui.R.

-   The little help buttons in the import dataset page have been moved to new locations. This can be done for other tabs.

-   Add "y axis" scrollbar to some tables which were exceeded screen boundaries in low resolutions.

-   Update contact email to new cnrs one.

### 1.0.5 : Minor changes and update caused by breaking changes in dependency

-   rfPerumte was updated and code had to be change to comply with new function names

-   The vignette was improved (mention of seed, and rendering adjustements)

### 1.0.6 Fixes

-   Changed url to diane.ipsim.inrae.fr added license to welcome pages

-   Fixed cutom gene info file upload in the case of splicing aware gene IDs in expression file

-   Fixed not displaying go results for custom org in network communities

-   In draw expression levels, genes filtered because of low expression can still be viewed

### 1.1 Multiple conditions DEG

-   It is now possible to perform DEA using multiple conditions

-   PCA plot readability has been improved

-   Now possible to plot a specific PCA in full screen

-   Now possible to download plots in the exploratory analysis pattern

-   Add a new PCA correlation plot

-   Add a pvalue histogram based on DEG list.

-   Now possible to display log2 of counts and to make the y axis start from 0.

-   Add visible version number

-   Add Alexandre Soriano in contact list

-   Add the possibility to give custom names to DEG list

### 1.2 Organism and datasets integration

-   New way to integrate custom organisms and custom datasets for the server version of DIANE. This is done by updating the organisms.rda and the integrated_datasets.rda files.

-   Add functions and a script to help in this process. 

- 	It's now possible to restrain or select organisms and datasets using URL parameters :
    - organism
    - organism_selected
    - data_selected


### 1.2.1 Add correlation heatmap

-   User can now see a correlation heatmap of their conditions

-   Add a new representation of count distribution, with all counts on the same plots.

-   A new low count removal method has been added.
