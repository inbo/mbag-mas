<!-- badges: start -->
![GitHub](https://img.shields.io/github/license/inbo/mbag-mas)
![GitHub repo size](https://img.shields.io/github/repo-size/inbo/mbag-mas)
<!-- badges: end -->

# MBAG - MAS (akkervogels)

[Langeraert, Ward![ORCID logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0002-5900-8109)[^aut][^cre][^inbo.be]
[Cartuyvels, Emma![ORCID logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0001-7856-6360)[^aut][^inbo.be]
[Van Calster, Hans![ORCID logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0001-8595-8426)[^aut][^inbo.be]
Instituut voor Natuur- en Bosonderzoek (INBO)[^cph][^fnd]

[^cph]: copyright houder
[^fnd]: financier
[^aut]: auteur
[^cre]: contactpersoon
[^inbo.be]: Instituut voor Natuur- en Bosonderzoek (INBO)

**keywords**: MBAG; MAS; akkervogels; biodiversiteit; beheerovereenkomsten; meetnet

<!-- community: inbo -->

### Beschrijving

<!-- description: start -->
De toestand van de biodiversiteit in het agrarisch gebied gaat achteruit, met negatieve gevolgen voor de biodiversiteit buiten het landbouwgebied, voor de landbouw zelf en voor de veerkracht tegenover klimaatverandering. Met het Meetnet Biodiversiteit Agrarisch Gebied (MBAG) trachten we deze toestand beter op te volgen, als basis voor gerichte maatregelen om de achteruitgang te keren. Dit deelproject legt de basis voor een meetnet voor agrarische soorten (MAS).

Dit in kaart brengen gebeurt op een snelle en efficiënte manier via puntentellingen, waarbij alle vogels en zoogdieren (o.a. haas, ree en vos) worden genoteerd binnen een straal van 300 m, op vier momenten tijdens het broedseizoen. In 2023 bestrijken we de Leemstreek, de Moeren en de Zandleemstreek, in 2024 willen we een meetnet uitrollen met zeggingskracht over heel Vlaanderen.
<!-- description: end -->

### Repo structuur

```
├── .github                        ├ Code of conduct, contributing and GitHub actions
├── data                           ├ input and processed data files, see individual README.txt files within this folder
├── inst
│   └── nl_be.dic                  ├ dictionary with words that should not be checked by the checklist package
├── media                          ├ media (often figures) created by scripts (often R Markdown)
├── output                         ├ output files (often datasets/pdf reports) created by scripts
├── source
│   ├── R                          ├ R script files, separate functions or small scripts
│   ├── bookdown                   ├ R bookdown files, scripts and files to create bookdown documents
│   ├── markdown                   ├ R Markdown files, small-medium report scripts
│   └── targets                    ├ R scripts with targets pipelines
├── .gitignore                     ├ files to ignore
├── .zenodo.json                   ├ zenodo metadata
├── CITATION.cff                   ├ citation info
├── LICENSE.md                     ├ licence
├── README.md                      ├ project description
├── checklist.yml                  ├ options checklist package (https://github.com/inbo/checklist)
└── mbag-mas.Rproj                 ├ R project
```
