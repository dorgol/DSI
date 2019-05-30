Code and data for visualizing Louise Kellogg's published work \
Jane Carlen \
May 2019

-----------------------------------------------------------------------------------------------------------------------------

The document **KelloggAuthorList_needs_author_cleaning_LJH.csv** (sorry for the misleading name) contains the data needed to make the plots. 

The R script **Kellogg_author_network.R** has the code to create an interactive and a dynamic plot. This script contains code that was used to clean the data in its sections #1 and #2, but this code does not need to be run again (section #0 should still be run to load packages, etc.). The clean data is in the KelloggAuthorList_needs_author_cleaning_LJH.csv, and that is loaded in section #3. Section #4 creats the plots

Cleaning tasks done:

1. About twenty papers were missing a year and a few more had a year in the early 1900's, well before her years of active work. 
    - Filled in the year for the papers missing years, unless a  duplicate of a paper someowhere else in the list, in which case filtered out.
    - Checked that the papers from years like 1914 should truly be removed.
    
2.  (Lorraine) cleaned author names, joining authors who are listed under different names
-   When you're done with your part, save a version of the to-be-cleaned csv with your initials in the title. 

**--> This is the file KelloggAuthorList_needs_author_cleaning_LJH.csv**


