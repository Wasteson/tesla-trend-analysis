# Narrative Template — Tesla Trend Analysis (DK)
# Version: 3.0
# Bruges som: metodedokument, agent-instruktion, og redaktionel guide
# Opdateres: ved hver ny kørsel — [AGENT]- og [VALIDERING]-felter udfyldes

---

## 1. Hvad denne analyse er — og ikke er

Denne analyse undersøger strukturelle skift i det danske brugtbilsmarked
med fokus på Tesla's adfærd relativt til sammenlignelige EV-brands.

**Data er:** Udbudsannoncer (asking prices) fra Bilbasen via TrackSights,
udtrukket fra `bilinfo.staging_bilinfo_single`. Dette er en FLOW-baseret
analyse — tidsaksen er listing creation date, ikke lagerøjebliksbillede.
Vi ser på nye annoncer der strømmer ind i markedet, ikke det samlede lager.

**Data er ikke:** Transaktionspriser. Vi observerer hvad sælgere prissætter
til — ikke hvad køber faktisk betalte. Alle priskonklusioner skal
præsenteres med dette forbehold eksplicit.

**Alle annoncer behandles som forhandlerannoncer.** SaleType er null for
~63% af Tesla-annoncer og ~58% af VW ID-annoncer — feltet er ikke
pålideligt til at skelne private fra erhverv. Da `RetailPriceWithoutTax`
kun dækker 3 Tesla-annoncer er filteret `PriceType = 'RetailPrice'`
reelt dækkende for hele det relevante univers.

**Kausalitetsdisciplin:** Event-markører (Musk/Trump-begivenheder)
annoteres som kontekst — ikke som forklaringer. Temporale sammenfald
er ikke evidens for effekt. Brug altid formuleringer som
"coinciding with", "in the weeks surrounding" — aldrig "caused by"
eller "following" i kausal betydning.

---

## 2. Sammenligningsgruppe — begrundelse

Tre kriterier for inklusion som sammenligningsbrand:
1. Samme prissegment (C/D-segment — mid-size to large family cars,
   broadly equivalent to Golf/Passat class, 300.000–600.000 DKK ny)
2. Tilstrækkelig volumen (median >= 10 listings/uge i analysepopulationen)
3. Forhandlersalg — så prisforskelle kan tilskrives brandspecifikke
   faktorer, ikke distributionsmodel

| Brand | Status | R-definition | Begrundelse |
|---|---|---|---|
| VW ID | Primær reference | `make == "VW" & str_detect(model, "^ID\\.")` | Højest volumen, samme segment, forhandlersalg |
| Hyundai Ioniq 5 | Primær reference | `make == "Hyundai" & model == "Ioniq 5"` | C/D-segment, forhandlersalg, stabil fra sep 2024 |
| Skoda Enyaq | Sekundær reference | `make == "Skoda" & model == "Enyaq"` | Lidt lavere prissegment — styrker konklusionen hvis mønster gentages |
| BYD | Særlig observation | `make == "BYD"`, kun fra 2025-01-01 | Ny aktør — inkluderes fra jan 2025 hvor volumen er stabil |
| BMW iX | Ekskluderet | — | For lav volumen; franchise-pricing confounder |
| Peugeot e-208 | Ekskluderet | — | Forkert prissegment (B-segment) |
| BYD pre-2025 | Ekskluderet | — | Median < 5 listings/uge før jan 2025 |

**Tesla-modeller:** Model 3 og Model Y dominerer. Model-mix er ikke
kontrolleret for — potentiel confounder der skal nævnes eksplicit i
metodesektionen.

---

## 3. Metodiske valg — eksplicitte begrundelser

**Startdato 1. juli 2024**
Datadækning i Bilbasen-kilden er ustabil før juli 2024. Tesla-volumen
er under 5 listings/uge i april-juni 2024 — inkludering af denne periode
ville destabilisere breakpoint-estimater for det tidlige analysevindue.
Startdato dokumenteres i appendix, ikke i hoveddelen.

**Julperioder ekskluderet**
Dec 20 – Jan 5 ekskluderes begge år (2024/25 og 2025/26) pga. anomalt
lav markedsaktivitet. Eksklusionen er konsekvent over begge år for at
undgå asymmetrisk behandling — en pause kun i ét år ville forvrænge
sammenligningen.

**Pris-trim: øverste 0.5%**
Fjerner åbenbare fejlposter og ekstremt eksklusive biler der ikke
repræsenterer det generelle EV-marked. Cutoff (~800.000 DKK) dokumenteres
i params_this_run.rds ved hver kørsel.

**Anomalidetektion: 4x median**
Uger med volumen over 4x segmentets median ekskluderes som sandsynlige
batch-imports. Alle eksklusioner rapporteres i CHANGELOG.
Eksempel: Tesla uge 24. august 2025 (1.296 listings vs. median 120).

**LOESS bandwidth: span = 0.3**
Valgt som balance mellem støjreduktion og responsivitet over for
strukturelle skift. Sensitivitetsanalyse med span = 0.2 og span = 0.5
bekræfter at primære konklusioner er robuste — dokumenteres i appendix.

**Segmenteret regression: 2 breakpoints**
To-breakpoint model foretrækkes fordi markedsdata typisk viser tre faser:
stabil periode, accelerationsfase, ny stabil periode. Konfidensintervaller
på breakpoints rapporteres via confint(). R² rapporteres men
kontekstualiseres eksplicit — høj R² er forventeligt på trendende
tidsserier og validerer ikke alene modelspecifikationen.

**Pearson + Spearman korrelation**
Pearson rapporteres for sammenlignelighed. Spearman rapporteres parallelt
som robusthedstjek — Pearson forudsætter linearitet, som ikke er garanteret
over det fulde volumeninterval. Begge metoder er enige i retning, hvilket
styrker konklusionerne.

**IQR som volatilitetsmål**
Ugentlig interkvartilrange (afstand mellem 25. og 75. percentil) på
retailpris. Robust over for outliers og kræver ingen fordelingsantagelser.

---

## 4. Analytiske konklusioner (validerede april 2026)

Disse konklusioner er valideret og må ikke overskrives uden ny eksplicit
validering af Øystein.

### 4.1 Markedskontekst
Alle brands viser samme brede mønster: vækst efterår 2024, pause ved
årsskiftet (december-effekt — ikke Trump-effekt), fornyet vækst ind i
2025, gradvis stabilisering. VW ID er klart størst og starter væksten
tidligere. Tesla og Skoda Enyaq følges tæt i volumen og timing.
BYD og Hyundai Ioniq 5 opererer på for lavt volumen til at kurveform
er meningsfuld.

### 4.2 Strukturelle skift (breakpoints)
- **Tesla:** Komprimeret transition — begge breakpoints inden for ~5 uger
  (okt-nov 2024, uge 15 og 20 fra juli 2024)
- **VW ID:** Gradvis transition, BP1 ~feb 2025 (uge 38), BP2 ~okt 2025
  (uge 72, bred CI — nær analyseperiodens grænse)
- **Hyundai Ioniq 5:** Komprimeret transition, men senere — mar-apr 2025
  (uge 38 og 42)
- **Skoda Enyaq:** Længst interval mellem breakpoints — gradvis
  markedscyklus (uge 37 og 67)

Ugenumre er relative til 1. juli 2024.
Uge 15 = mid-oktober 2024, uge 20 = mid-november 2024,
uge 38 = mid-marts 2025.

### 4.3 Prisadfærd
- Tesla er det eneste brand med nær-nul pris-volumen korrelation
  (Pearson r = 0.06, p = 0.57; Spearman rho = -0.19, p = 0.07)
- Alle andre brands viser signifikant negativ korrelation
  (mere listings -> lavere priser)
- Hyundai Ioniq 5 er en mellemting — ikke så negativ som VW ID/Skoda,
  ikke nær nul som Tesla
- Hyundai's bratte negative linje i scatter skyldes smalt volumeninterval
  (0-100/uge) — artefakt af begrænset volumenvariation, ikke ægte signal
- Tesla's prisforløb er det mest stabile af alle EV-brands over perioden

### 4.4 Musk/Trump-vinklen
- H2 2024 er en Tesla-specifik periode med forhøjet prisspredning (IQR)
- Tesla's IQR stiger kraftigt fra juli 2024 — den stejleste stigning i
  hele grafen — topper omkring valg og indsættelse, falder gradvist
  gennem 2025
- VW ID og Skoda Enyaq er relativt stabile i H2 2024, stiger begge fra 2025
- Alle tre brands konvergerer mod samme niveau sent 2025
- Samme mønster er synligt i rapriserne (11_appendix_raw_price) —
  Tesla viser markant større prisudsving end peers i H2 2024
- Tolkning: sælgere var uenige om hvad en Tesla var værd i perioden.
  Konsistent med Musks politiske profil som voksende faktor fra juli 2024,
  men også konsistent med dynamikkerne i et hurtigt voksende marked.
  Kausalitet kan ikke etableres fra listing-data.
- Pa tvaers af alle tre event-vinduer bevægede Tesla's IQR sig modsat VW ID:

| Event | Tesla | VW ID |
|---|---|---|
| Musk backs Trump (jul 2024) | op | ned |
| Trump elected (nov 2024) | ned | op |
| Trump inaugurated (jan 2025) | ned | op |

---

## 5. Godkendt graf-liste (april 2026)

| Graf | Fil | Sektion | Status |
|---|---|---|---|
| Total market LOESS | 01_market_overview_loess | 3 | Bevares |
| Brand comparison LOESS | 02_brand_comparison_loess | 4.1 | Bevares |
| LOESS sensitivity | 03_loess_sensitivity | — | DROPPES |
| Segmented Tesla | 04_segmented_tesla | 5.1 | Bevares |
| Segmented VW ID | 04_segmented_vw_id | 5.1 | Bevares |
| Segmented Skoda Enyaq | 04_segmented_skoda_enyaq | 5.1 | Bevares |
| Segmented Hyundai Ioniq 5 | 04_segmented_hyundai_ioniq_5 | 5.1 | Bevares |
| Breakpoint timing | 05_breakpoint_timing | 5.2 | Bevares |
| Correlation price-volume | 06_correlation_price_volume | 6.1 | Bevares |
| Scatter volume-price | 07_scatter_volume_price | 6.2 | Bevares |
| Price LOESS | 08_price_loess | 6.3 | Bevares — fossil droppes fra graf |
| Volatility IQR | 09_volatility_iqr | 7.2 | Bevares — Skoda erstatter Hyundai |
| Event: Musk backs Trump | 10_event_volatility_musk_backs_trump | — | DROPPES |
| Event: Trump elected | 10_event_volatility_trump_elected | — | DROPPES |
| Event: Trump inaugurated | 10_event_volatility_trump_inaugurated | — | DROPPES |
| Raw prices appendix | 11_appendix_raw_price | 7.2 + appendix | Loftes til sektion 7 |

---

## 6. Obligatoriske caveats (ma aldrig udelades)

- [ ] **Data note:** "All prices reflect retail asking prices — what
      sellers list, not confirmed transaction prices."
- [ ] **Flow note:** "The time axis reflects listing creation date —
      when new listings entered the market — not total inventory at
      any point in time."
- [ ] **Causality note:** Eksplicit forbehold ved alle event-markorer
- [ ] **Coverage note:** Hvis Hyundai Ioniq 5 eller BYD indgar i
      breakpoint-analyse — note om begraenset volumen i tidlig periode
- [ ] **LOESS note:** Bandwidth-valg og sensitivitetsanalyse naevnt

---

## 7. Mappestruktur (malbillede)

tesla analyse/
  R/
    01_data.R
    02_charts.R
    03_git_push.R
  data/
    market_data.rds
    weekly_volume.rds
    weekly_price.rds
    events.rds
    params_this_run.rds
    json/
      loess_volume.json
      loess_price.json
      segmented_breakpoints.json
      correlation_table.json
      iqr_over_time.json
      raw_prices.json
  docs/
    index.html
    output/
  output/
  params.yml
  narrative_template.md
  validation_checklist.md
  CHANGELOG.md

---

## 8. [AGENT] — udfyldes ved hver korsel

Analysekørsel: [DATO]
Periode daekket: [START] -> [SLUT]
Antal uger i analysen: [INDSAET]
n Tesla total: [INDSAET]
n VW ID total: [INDSAET]
Anomalier ekskluderet denne korsel: [INDSAET eller "ingen"]
Breakpoints Tesla BP1/BP2 (uger): [INDSAET]
Breakpoints VW ID BP1/BP2 (uger): [INDSAET]
Pearson r Tesla: [INDSAET]
Nye events der bor overvejes: [INDSAET eller "ingen"]
Tesla-narrativ denne korsel: [2-3 saetninger]
Storste metodiske forbehold denne korsel: [1-2 saetninger]
Godkendt af Oystein: [JA/NEJ]

---

## 9. [VALIDERING] — tjekpunkter for publicering

- [ ] Sanity checks i 01_data.R er laest og godkendt
- [ ] Anomalier er vurderet og dokumenteret i CHANGELOG
- [ ] Breakpoints er konsistente med forrige korsel eller aendring er forklaret
- [ ] Konfidensintervaller pa breakpoints er med i output
- [ ] Spearman-korrelation er rapporteret parallelt med Pearson
- [ ] LOESS-sensitivitetsanalyse er kort og konklusioner er robuste
- [ ] Graf-liste matcher godkendt liste i sektion 5
- [ ] Alle obligatoriske caveats er til stede i teksten
- [ ] Ingen kausalitetssprog i tekst eller figurtitler
- [ ] Alle event-datoer har kildeangivelse
- [ ] CHANGELOG er udfyldt med denne korsel

---

Tesla Trend Analysis · TrackSights Analytics · Version 3.0 · April 2026
