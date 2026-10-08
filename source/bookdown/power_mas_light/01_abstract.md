::: {lang=en-GB}

# English abstract {-}
A monitoring network, which focuses on common farmland birds (known as the 'Meetnet Agrarische Soorten', MAS), was rolled out across Flanders in 2024 and 2025 as part of the Agricultural Area Biodiversity Monitoring Network (known as the 'Meetnet Biodiversiteit Agrarisch Gebied', MBAG).
The primary objective of the MAS is to evaluate the effectiveness of restoration measures by comparing population trends between areas with and without restoration.
In addition, MAS survey points can contribute to monitoring the national farmland bird index in support of the EU Nature Restoration Regulation [@vangossum2026natuurherstelverordening].

This report does not focus on the national index, but specifically assesses the statistical feasibility of a simplified monitoring design ("MAS light") to demonstrate differences in population trends of common farmland birds between Species Protection Programme (SPP) areas and comparable areas outside SPPs.
The central question is what difference in trend between both groups can be detected with sufficient statistical confidence.
A smaller minimally detectable trend difference is more favourable in this context, because it means that smaller differences in population trend are also statistically demonstrable.

To address this question, a simulation-based power analysis was conducted. Count data were generated using a Poisson generalized linear mixed model (GLMM) and subsequently analysed using the same model. The minimum detectable effect (MDE) was estimated for scenarios with 100, 200, and 400 survey points for a time series of 10, 16, and 24 years. The effect size was defined as the smallest detectable difference in population trend between SPP and non-SPP areas.

The simulations show that the detectable effect size decreases as both the number of survey points ($n$) and the length of the time series ($T$) increase, and is approximately proportional to $1/\sqrt{nT^3}$. The analyses were conducted for a hypothetical farmland bird species with a high abundance and limited variation between survey points. The results are therefore not directly representative of all farmland bird species; for rarer species or species with a more heterogeneous distribution, the minimum detectable effect size will generally be larger.

Even for this hypothetical farmland bird species, the minimum detectable difference in population trend remains relatively large. In a monitoring scheme with 400 survey points, only around 200 survey points may, in practice, provide informative observations for a realistic species that does not occur everywhere. Under these conditions, only a substantially more favourable trend within SPP areas than outside SPP areas can be detected. Specifically, over a 10-year period, the population within SPP areas would need to show a trend 58 % more favourable than that outside SPP areas before a statistically significant difference could be demonstrated. For example, if the population declines by 9.5 % outside SPP areas over 10 years, it would need to increase by 46 % within SPP areas. If the population remains stable outside SPP areas, it would need to increase by 58 % within SPP areas, while an increase of 10.5 % outside SPP areas would require an increase of 73 % within SPP areas to yield a statistically significant difference. For a 24-year time series, the required difference is smaller: the population within SPP areas would need to exhibit a trend that is 38 % more favourable than that outside SPP areas before a statistically significant difference could be detected.

An additional sensitivity analysis shows that the minimum detectable effect decreases with increasing variation between survey points, as a result of the increase in marginal expected abundance under a log link.
When setting the variance parameter for future power analyses, we therefore recommend taking a cautious approach and setting it higher rather than lower.
However, this sensitivity analysis does not change the main conclusion of the power analysis.

These results indicate that reducing the number of survey points is not advisable, as doing so would substantially reduce the statistical power of the monitoring scheme and limit its ability to reliably detect differences in population trends between SPP and non-SPP areas.
:::

<!-- This part adds the table of content in the pdf -->
<!-- Add it at the end of the last chapter of the frontmatter -->
<!-- spell-check: ignore:start-->
::: {.content-visible when-format="pdf"}
\clearpage
\phantomsection
\addcontentsline{toc}{chapter}{\contentsname}
\setcounter{tocdepth}{2}
\tableofcontents

<!-- keep the lines below -->
:::
<!-- spell-check: ignore:end-->
<!-- This part adds the tables of contents in the pdf -->
