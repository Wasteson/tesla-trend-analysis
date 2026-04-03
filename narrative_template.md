# Narrative Template — Tesla Trend Analysis (DK)
# Version: 2.0
# Bruges som: metodedokument, agent-instruktion, og redaktionel guide
# Opdateres: ved hver ny kørsel — [AGENT]- og [VALIDERING]-felter udfyldes

---

## 1. Hvad denne analyse er — og ikke er

Denne analyse undersøger strukturelle skift i det danske brugtbilsmarked
med fokus på Tesla's adfærd relativt til sammenlignelige EV-brands.

**Data er:** Udbudsannoncer (asking prices) fra Bilbasen via TrackSights.
Dette er en FLOW-baseret analyse — tidsaksen er listing creation date,
ikke lagerøjebliksbillede. Vi ser på nye annoncer der strømmer ind i
markedet, ikke det samlede lager.

**Data er ikke:** Transaktionspriser. Vi observerer hvad sælgere
prissætter til — ikke hvad køber faktisk betalte. Alle priskonklusioner
skal præsenteres med dette forbehold eksplicit.

**Kausalitetsdisciplin:** Event-markører (Musk/Trump-begivenheder)
annoteres som kontekst — ikke som forklaringer. Temporale sammenfald
er ikke evidens for effekt. Brug altid formuleringer som
"coinciding with", "in the weeks surrounding", aldrig "caused by"
eller "following" i kausal betydning.

---

## 2. Sammenligningsgruppe — begrundelse

Tre kriterier for inklusion som sammenligningsbrand:
1. Samme prissegment (C/D-segment, 300.000–600.000 DKK ny)
2. Tilstrækkelig volumen (median >= 10 listings/uge i analysepopulationen)
3. Forhandlersalg (ikke direct-to-consumer) — så prisforskelle kan
   tilskrives brandspecifikke faktorer, ikke distributionsmodel

| Brand | Status | Begrundelse |
|---|---|---|
| VW ID | Primær reference | Højest volumen, samme segment, forhandlersalg |
| Hyundai Ioniq 5 | Primær reference | C/D-segment, forhandlersalg, stabil volumen fra sep 2024 |
| Skoda Enyaq | Sekundær reference | Lidt lavere prissegment — styrker konklusionen hvis mønster gentages |
| BYD | Særlig observation | Ny aktør — inkluderes fra jan 2025 hvor volumen er stabil |
| BMW iX | Ekskluderet | For lav volumen; franchise-pricing confounder |
| Peugeot e-208 | Ekskluderet | Forkert prissegment (B-segment) |
| Hyundai Ioniq 6 | Ekskluderet | Lav volumen — slås ikke sammen med Ioniq 5 |

---

## 3. Metodiske valg — eksplicitte begrundelser

**Startdato 1. juli 2024**
Datadækning i Bilbasen-kilden er ustabil før juli 2024. Lav ugentlig
volumen for Tesla (median 3/uge i april-juni 2024) giver upålidelige
breakpoint-estimater. Analysen starter konsekvent 1. juli 2024.

**Julperioder ekskluderet**
Dec 20 – Jan 5 ekskluderes begge år (2024/25 og 2025/26) pga. anomalt
lav markedsaktivitet. Eksklusionen er konsekvent over begge år for
at undgå asymmetrisk behandling af datasættet.

**Pris-trim: øverste 0.5%**
Fjerner åbenbare fejlposter og ekstremt eksklusive biler der ikke
repræsenterer det generelle EV-marked. Cutoff dokumenteres i
params_this_run.rds ved hver kørsel.

**Anomalidetektion: 4x median**
Uger med volumen over 4x segmentets median ekskluderes som sandsynlige
batch-imports. Alle eksklusioner rapporteres i CHANGELOG.

**LOESS bandwidth: span = 0.3**
Valgt som balance mellem støjreduktion og responsivitet.
Sensitivitetsanalyse med span = 0.2 og span = 0.5 præsenteres i appendix
for at vise at konklusionerne er robuste over for bandwidth-valget.

**Segmenteret regression: 2 breakpoints**
To-breakpoint model foretrækkes frem for én fordi markedsdata typisk
viser tre faser: stabil periode, accelerationsfase, ny stabil periode.
Konfidensintervaller på breakpoints rapporteres via confint().
R² rapporteres men kontekstualiseres — høj R² er forventeligt på
trendende tidsserier og er ikke alene bevis for god modelspecifikation.

**Pearson vs. Spearman korrelation**
Pearson rapporteres for sammenlignelighed med original analyse.
Spearman rapporteres parallelt som robusthedstjek — Pearson forudsætter
linearitet, som ikke er garanteret i denne datakontekst.

---

## 4. Event-volatilitetsanalyse — Musk/Trump-vinklen

**Hypotese (testbar):**
Teslas asking-price volatilitet (målt som ugentlig IQR) øgedes i ugerne
omkring højtprofilerede Musk/Trump-begivenheder, relativt til:
(a) Teslas egen pre-event baseline, og
(b) VW ID og Hyundai Ioniq 5 i de samme uger

**Operationalisering:**
- Volatilitetsmål: ugentlig IQR på retailpris (tilgængeligt i weekly_price.rds)
- Event-vinduer: ±3 uger omkring hver event-dato
- Sammenligning: Tesla vs. VW ID side om side i samme vindue

**Hvad vi kan konkludere:**
Deskriptivt mønster — synligt eller ikke synligt. Ingen kausalitetspåstand.
Korrekt formulering: "Tesla's price volatility was elevated in the weeks
surrounding [event], a pattern not observed for VW ID or Hyundai Ioniq 5."

**Hvad vi ikke kan konkludere:**
At begivenhederne *forårsagede* volatiliteten. Mix-effekter, sæsonalitet
og generel markedsusikkerhed er alternative forklaringer der ikke kan
udelukkes med listing-data alene.

---

## 5. Obligatoriske afsnit i hver publiceret version

Følgende afsnit må ikke udelades — de er metodisk og redaktionelt
nødvendige for at analysen er citerbar:

- [ ] **Data note:** "This analysis is based on asking-price listings
      from Bilbasen via TrackSights. Prices reflect what sellers list,
      not confirmed transaction prices."
- [ ] **Flow note:** "The time axis reflects listing creation date —
      when new listings entered the market — not total inventory at
      any point in time."
- [ ] **Causality note:** Eksplicit forbehold ved alle event-markører
- [ ] **Coverage note:** Hvis Hyundai Ioniq 5 eller BYD indgår i
      breakpoint-analyse — note om begrænset volumen i tidlig periode
- [ ] **LOESS note:** Bandwidth-valg og sensitivitetsanalyse nævnt

---

## 6. [AGENT] — udfyldes ved hver opdatering
```
Analysekørsel: [DATO]
Periode dækket: [START] → [SLUT]
Nye events siden sidst: [BESKRIV ELLER "ingen"]
Breakpoints ændret væsentligt: [JA/NEJ — beskriv hvis ja]
Tesla-narrativ denne kørsel: [2-3 sætninger]
Største metodiske forbehold denne kørsel: [1-2 sætninger]
Godkendt af Øystein: [JA/NEJ]
```

---

## 7. [VALIDERING] — tjekpunkter før publicering

- [ ] Alle sanity checks i validation_checklist.md er godkendt
- [ ] Konfidensintervaller på breakpoints er med i output
- [ ] Spearman-korrelation er rapporteret parallelt med Pearson
- [ ] LOESS-sensitivitetsanalyse er kørt og konklusioner er robuste
- [ ] Alle event-datoer har kildeangivelse
- [ ] Ingen kausalitetssprog i tekst eller figurtitler
- [ ] Data note, Flow note og Causality note er til stede