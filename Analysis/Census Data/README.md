# How Census Data was Extracted from U.S. Census Bureau County Intercensal Datasets (1990-2019) 
1.	We will not include the “two or more races” group as this group is insignificantly small and wasn’t included in the 1995 data. Eliminate every column from the data corresponding to the year of interst from the "Originally Downloaded Datasets" folder except:
    - COUNTY
    - YEAR
    - AGEGRP
    - NHWA_MALE
    - NHWA_FEMALE
    - NHBA_MALE
    - NHBA_FEMALE
    - NHIA_MALE
    - NHIA_FEMALE
    - NHAA_MALE
    - NHAA_FEMALE
    - NHNA_MALE
    - NHNA_FEMALE
    - H_MALE
    - H_FEMALE
2.	Select “Format as Table” under the Style tab of the Home tab. 
3.	Select the drop down next to YEAR and select the year of interest. Use the “Definitions” document to find the number that corresponds to the year of interest. 
4.	Create a new sheet within that document, title it corresponding year, and copy and paste the values only from the original sheet into this new sheet. Then, delete the original sheet. 
5.	Create two new columns titled AI_MALE and AI_FEMALE, respectively. Then, set them equal to the values only of the sum of the NHNA and NHIA columns upon which these 4 columns can be hidden. This is to ensure that both American Indian and Native Hawaiians are included in the same group. 
6.	Use the “Transpose Template” .xlsx document to paste the formulas from the sheet into your sheet. 
7.	Then, copy the values only of the results from the formula in addition to the “AGEGRP” column into a new sheet called “YEAR Extraction”. Format these 4 columns as a table. Then, use the dropdown next to “AGEGRP” to deselect every number that isn’t the numbers 0-9. 
a.	This is done because only the calculations from the first 10 rows for each county correspond to the correct groups from the transpose document. 
8.	Copy the data from the table and paste it into the SIR calculations .xlsx

