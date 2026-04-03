# Validation Checklist — Tesla Trend Analysis
# Køres efter 01_data.R og før 02_charts.R
# Arkitekten (Øystein) godkender alle punkter inden charts produceres

---

## 1. Datadækning

- [ ] Periode starter 2024-07-01 eller senere
- [ ] Periode slutter inden for de seneste 7 dage (Sys.Date())
- [ ] Ingen manglende priser (missing_price = 0)
- [ ] Julperioder er ekskluderet (dec 20 – jan 5 begge år)

## 2. Segmentvolumen — minimumskrav

- [ ] Tesla: median >= 50 listings/uge
- [ ] VW ID: median >= 100 listings/uge
- [ ] Skoda Enyaq: median >= 30 listings/uge
- [ ] Hyundai Ioniq 5: median >= 15 listings/uge (lav i jul-aug 2024 — acceptabelt)
- [ ] BYD: median >= 10 listings/uge (kun fra jan 2025)

## 3. Anomalidetektion

- [ ] Anomalirapport er læst og vurderet
- [ ] Eventuelle anomalier er identificeret som batch-import eller teknisk fejl
- [ ] Ingen anomalier er ekskluderet uden begrundelse i CHANGELOG

## 4. Prisdata

- [ ] Pris-cutoff (99.5%-percentil) er rimelig — typisk 750.000–850.000 DKK
- [ ] Ingen negative eller nul-priser
- [ ] IQR og SD er tilgængelige i weekly_price.rds (bruges til volatilitetsanalyse)

## 5. Events

- [ ] Alle event-datoer er bekræftede med kildeangivelse
- [ ] "Tesla EU sales rebound" dato er opdateret med præcis dato fra Finansavisen

## 6. Godkendelse

Dato: _______________
Godkendt af: _______________
CHANGELOG-entry udfyldt: [ ]