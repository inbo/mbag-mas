{{< colophon >}}

# Samenvatting {.unnumbered}

<!-- description: start -->
Het Meetnet Agrarische Soorten (MAS), dat op algemene boerenlandvogels focust, werd in 2024 en 2025 over heel Vlaanderen uitgerold als onderdeel van het Meetnet Biodiversiteit Agrarisch Gebied (MBAG).
Het primaire doel van het MAS is het evalueren van de doeltreffendheid van herstelmaatregelen door populatietrends te vergelijken tussen gebieden met en zonder herstel.
Daarnaast kunnen MAS-telpunten bijdragen aan het opvolgen van de nationale boerenlandvogelindex, ter ondersteuning van de EU Natuurherstelverordening [@vangossum2026natuurherstelverordening].

Dit rapport focust niet op de nationale index, maar onderzoekt specifiek de statistische haalbaarheid van een vereenvoudigd monitoringsontwerp ("MAS light") om verschillen in populatietrends van algemene boerenlandvogels tussen soortenbeschermingsprogrammagebieden (SBP-gebieden) en vergelijkbare gebieden buiten SBP's aan te tonen.
De centrale vraag is welk verschil in trend tussen beide groepen met voldoende statistische zekerheid kan worden aangetoond.
Een kleiner minimaal detecteerbaar trendverschil is hierbij gunstiger, omdat dit betekent dat ook kleinere verschillen in populatietrend statistisch aantoonbaar zijn.

Hiervoor werd een simulatiegebaseerde poweranalyse uitgevoerd, waarbij telgegevens werden gegenereerd met een Poisson GLMM (*generalized linear mixed model*) en vervolgens geanalyseerd met hetzelfde model. De minimaal detecteerbare effectgrootte (MDE) werd bepaald voor verschillende scenario's met 100, 200 en 400 telpunten en tijdreeksen van 10, 16 en 24 jaar. De effectgrootte wordt hierbij gedefinieerd als het minimaal detecteerbare verschil in populatietrend tussen SBP- en niet-SBP-gebieden.

De simulaties tonen dat de detecteerbare effectgrootte afneemt naarmate zowel het aantal telpunten ($n$) als de lengte van de tijdreeks ($T$) toeneemt en bij benadering evenredig is met $1 / \sqrt{nT^3}$. De analyses zijn uitgevoerd voor een hypothetische boerenlandvogelsoort met een hoge dichtheid en een beperkte variatie tussen telpunten. De resultaten zijn daarmee niet rechtstreeks representatief voor alle boerenlandvogels; voor zeldzamere soorten of soorten met een heterogenere verspreiding zal de minimaal detecteerbare effectgrootte doorgaans groter zijn.

Zelfs voor deze hypothetische boerenlandvogelsoort blijft het minimaal detecteerbare trendverschil relatief groot. Bij een meetnet van 400 telpunten zijn, voor een realistische soort die niet overal voorkomt, in de praktijk mogelijk slechts ongeveer 200 telpunten informatief. In dat geval is pas een sterk gunstiger trend binnen SBP-gebieden dan buiten SBP-gebieden detecteerbaar. Concreet betekent dit dat de populatie binnen SBP-gebied over een periode van 10 jaar 58 % gunstiger moet evolueren dan buiten SBP-gebied voordat een statistisch significant verschil in trend kan worden aangetoond. Indien deze soort 9,5 % buiten SBP-gebied afneemt over 10 jaar, moet ze binnen SBP-gebied 46 % toenemen. Voor een stabiele trend buiten SBP-gebied moet ze binnen SBP-gebied 58 % toenemen en voor een toename van 10,5 % buiten SBP-gebied moet ze 73 % toenemen binnen SBP-gebied.
Beschouw hetzelfde voorbeeld voor een tijdreeks van 24 jaar; dan moet de populatie binnen SBP-gebied 38 % gunstiger evolueren dan buiten SBP-gebied voordat een statistisch significant verschil in trend kan worden aangetoond.

Een aanvullende sensitiviteitsanalyse toont aan dat het minimaal detecteerbare effect afneemt bij een grotere variatie tussen telpunten, als gevolg van de toename in marginale verwachte abundantie bij een log-link.
Deze sensitiviteitsanalyse wijzigt de hoofdconclusie van de poweranalyse echter niet.

De resultaten wijzen er dan ook op dat het verkleinen van het aantal meetpunten niet aangewezen is, omdat de statistische gevoeligheid dan te beperkt wordt om verschillen in populatietrend tussen SBP- en niet-SBP-gebieden betrouwbaar aan te tonen.
<!-- description: end -->
