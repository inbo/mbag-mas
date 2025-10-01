README
======

This folder contains data files related to the sampling design and framework for the MAS (Meetnet Agrarische Soorten) monitoring projects. These files are used in both the MAS pilot project and the MBAG MAS project.

For more information and the scripts used to generate these files, see:
- source/markdown/verzameling_steekproefkaders_mbag.Rmd in the MBAG MAS repository: https://github.com/inbo/mbag-mas
- src/markdown/general/uitschrijven_steekproefkaders_mbag.Rmd in the MAS pilot repository: https://github.com/inbo/mas-piloot

Contents
--------

1. Sampling Framework
   - steekproefkader_mbag_piloot.csv: sampling frame used in the MAS pilot project
   - steekproefkader_mbag_mas.csv: sampling frame used in the MBAG MAS project

2. Stratum Weights per Agricultural Region
   - Files named gewichten_XXX.csv, where XXX refers to the agricultural region
   - Contains weights for each stratum within the sampling frame

3. Sample
   - steekproef_mbag_piloot.csv: sample drawn for the MAS pilot project
   - steekproef_mbag_mas.csv: sample drawn for the MBAG MAS project

4. Final Sample After Manual Validation
   - Locations that were actually surveyed in the field
   - steekproefkader_avimap_mbag_piloot.csv: final validated sample for the pilot project
   - steekproefkader_avimap_mbag_mas.csv: final validated sample for MBAG MAS
