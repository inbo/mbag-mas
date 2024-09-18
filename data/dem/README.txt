This folder contains the following data files related to elevation in Flanders.

- DHMVIIDSMRAS5m.tif
  - DSM input file
  - Downloaded from: https://www.vlaanderen.be/digitaal-vlaanderen/onze-oplossingen/earth-observation-data-science-eodas/het-digitaal-hoogtemodel/digitaal-hoogtemodel-vlaanderen-ii
  - Used to calculate strata openness of landscape for sampling frame and for calculation of openness variable (see further)
  - Used directly to calculate visibility at sampling locations for sampling frame
- DHMVIIDTMRAS5m.tif
  - DTM input file
  - Downloaded from: https://www.vlaanderen.be/digitaal-vlaanderen/onze-oplossingen/earth-observation-data-science-eodas/het-digitaal-hoogtemodel/digitaal-hoogtemodel-vlaanderen-ii
  - Used to calculate strata openness of landscape for sampling frame and for calculation of openness variable (see further)
  - Used directly to calculate visibility at sampling locations for sampling frame
- vlaanderen_chm5.tif
  - Intermediate file for calculation of openness
  - Difference calculated DSM - DTM
  - See landschap_dsm_openheid.R in MAS pilot repository

- openness300m_chm_res5_vlaanderen.tif
  - Intermediate file for calculation of openness on 300 m scale
  - Calculation of openness from vlaanderen_chm5.tif
  - See landschap_dsm_openheid.R in MAS pilot repository
- openness300m_chm_res25_vlaanderen.tif
  - Openness300m_chm_res5_vlaanderen.tif aggregated to 25 m resolution
  - See landschap_dsm_openheid.R in MAS pilot repository
- openness300m_chm_res25_c300_mean_vlaanderen.tif
  - Average openness in 300 m radius landscape
  - See landschap_dsm_openheid.R in MAS pilot repository
  - Used to calculate strata openness of landscape for sampling frame
- openness300m_chm_res25_c300_mean_vlaanderen_classified.tif
  - Categorised openness300m_chm_res25_c300_mean_vlaanderen.tif
  - See landschap_dsm_openheid.R in MAS pilot repository
  
- openness1000m_chm_res5_vlaanderen.tif
  - Intermediate file for calculation of openness on 1000 m scale
  - Calculation of openness from vlaanderen_chm5.tif
  - See landschap_chm_openheid_1000m.R in MAS pilot repository
- openness1000m_chm_res25_vlaanderen.tif
  - openness1000m_chm_res5_vlaanderen.tif aggregated to 25 m resolution
  - See landschap_chm_openheid_1000m.R in MAS pilot repository
- openness1000m_chm_res25_c1000_mean_vlaanderen.tif
  - Average openness in 300 m radius landscape
  - See landschap_chm_openheid_1000m.R in MAS pilot repository
  - Used for calculation of openness variable
- openness1000m_chm_res25_c1000_mean_vlaanderen_classified.tif
  - Categorised openness1000m_chm_res25_c1000_mean_vlaanderen.tif
  - See landschap_chm_openheid_1000m.R in MAS pilot repository

MAS pilot repository: https://github.com/inbo/mas-piloot
MBAG MAS repository: https://github.com/inbo/mbag-mas