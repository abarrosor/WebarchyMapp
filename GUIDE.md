# WebarchyMapp — user guide

A personal R/RStudio project that downloads official open data about citizen safety in Spain, loads it into a local DuckDB database and shows it in a Shiny app with maps. Everything runs on your computer; nothing is sent anywhere and nothing costs money.

## 1. What the app shows

| View | Data | Level | Source |
|---|---|---|---|
| **Spain** | Known criminal offences by region, province and municipality (> 20,000 inhabitants); in Catalonia also by police area (Mossos) | Quarter / year | Ministry of the Interior (Crime Balance); Mossos d'Esquadra; population INE; boundaries IGN |
| **Cities › Madrid** | Calls received by the Municipal Police by neighbourhood; police activity by district; arrests by motive (city) | Month | Madrid City Council (datos.madrid.es) |
| **Cities › Barcelona** | Incidents handled by the City Police by neighbourhood; recorded crime by district (Mossos) | Month | Barcelona City Council (Open Data BCN); Mossos |

Every figure is shown with its source, what it measures and its limitations. Different sources are **never added together or compared with each other**: a call is not a crime, and Madrid's categories are not Barcelona's.

## 2. First time

1. Install R (4.3 or newer) and RStudio.
2. Open `WebarchyMapp.Rproj` in RStudio.
3. Open `WEBARCHYMAPP.R` and press **Source** (Ctrl + Shift + S). A menu appears in the console.
4. Choose **5** to install the R packages (once).
5. Choose **1** to download and load everything. The first run takes a while (Crime Balance since 2019, 150+ Excel files of Madrid activity, Mossos since 2019...). Downloads are not re-done on later runs.
6. Choose **2** to open the app (English) or **3** to pick another language.

## 3. The menu (`WEBARCHYMAPP.R`)

| Option | What it does |
|---|---|
| 1 | **Update everything**: downloads what is new from every source, loads it, rebuilds the app tables and prints the data status. Safe to repeat |
| 2 | Open the app in English |
| 3 | Open the app in another language (Spanish, Catalan, Galician, French, Italian, German, Portuguese, Chinese, Japanese, Korean) |
| 4 | Show the data status (months loaded per source, unmapped types, missing pieces) |
| 5 | Install or complete the R packages |
| 6 | Test the datos.madrid.es API |
| 7 | Download the official boundaries again and reload districts and neighbourhoods |
| 8 | Update only Spain, Barcelona and Mossos (not the Madrid sources) |
| 9 | Schedule the automatic weekly update (Windows Task Scheduler; runs `Rscript` without opening RStudio) |
| 10 | Remove the automatic update |
| 11 | Check whether the update is scheduled and how the last run went (`reports/update_*.log`) |
| 12 | Rebuild the database from scratch from `data/raw/` (originals untouched) |
| 13 | **Update only the other countries** (the *Country* menu of the app) and the Eurostat tables; option 1 includes them. A country whose raw and reference files did not change since its last load is kept as it is (Canada alone parses 50 GB); to reload one after a code fix: option 14, or `update_countries(only = "CA")` in the console |
| 14 | **Reload some countries**: asks for the codes (`GBE GBS FI`, separated by spaces, no quotes) and reloads them even if their files did not change. Use it after a fix to one module |
| 15 | **Check the consistency of the other countries' data**: duplicates, territories without name, cumulative figures that decrease, parts above the total, population coverage, polygons without territory, quarters in the future. Prints the findings and saves `reports/country_checks_<date>.csv`. Read-only |

The same actions exist as scripts in `scripts/` and `scripts/maintenance/`.

## 4. Using the app

**Cities (Madrid, Barcelona)**
- *Filters*: category (public safety / nuisance), type of call or incident (only those with events in the period are listed, with counts), period slider with *This year · 5 years · All* views, shading by rate (default) or number.
- *Map*: hover a neighbourhood for a summary; click it (or search it) to open its **profile**: events, rate per 1,000 residents, comparison with the district and the city, monthly bars, types, and — in Madrid — the Municipal Police activity in the district; in Barcelona, the recorded crime of the district (Mossos).
- *Statistics*: top 5 districts for the current filters, with the city average; in Madrid, people arrested or investigated by motive (whole city).

**Country menu (Spain and the other countries)**
- The top bar has a *Country* menu with every country that has data loaded (Spain always; the others after option 1 or 13). Each country is a view of its own, with the levels its source publishes (for instance France: régions, départements, communes; Germany: Länder and Kreise; Ireland: Garda divisions only). The controls below are the same for every country, and **no figure of one country is ever compared with another**: definitions and recording rules differ.
- *Countries (table)*: the only place where several countries appear together — national rates per 100,000 inhabitants as published by Eurostat (`crim_off_cat`, ICCS categories), with a warning. Only European countries (Eurostat's scope); UNODC's data for the Americas are not redistributed because its terms do not allow it.
- A country whose source has no boundaries file (or whose download failed) shows tables instead of a map.

**Spain**
- *Level*: regions, provinces or municipalities. With **Follow the map** (on by default) the level follows the zoom (all of Spain → regions; a few provinces → provinces; closer → municipalities), the municipalities of every region in view are shown (only those above 20,000 inhabitants, which are the ones with data), and the territory list and the table only contain what is inside the view. Untick it to pick a region by hand; in Catalonia the *Source* control then lets you switch to the Mossos police areas.
- *Type of offence* lists every offence type with figures in the period, ordered by count. With every type ticked the figure is the Ministry's official **total** (which also includes the sensitive types never listed: homicides, sexual offences, kidnapping...); with a subset it is the sum of the ticked types. *All · None* and clicking a type in the breakdown table change the selection.
- *Period* works like the city slider: *Latest year · 5 years · All* chips and a two-handle slider over the years. One handle position shows one year (the latest, partial one is listed on its own); a range **adds up full years** and the rates become **yearly averages**, so that they stay comparable with single years. The partial year is left out of any range.
- Each profile has a **rate comparison** card: how the rate compares with the reference (Spain, the region or the city) in five steps from "much lower" to "much higher" (ratio below 0.6, 0.6–0.85, 0.85–1.15, 1.15–1.5, above 1.5), the % difference and the position among the territories of the same level. It compares recorded figures per resident; it is not a safety or danger score. Click a territory on the map or in the table to open its profile: offences, rate, ratio to the reference (Spain or the region), breakdown by type and yearly evolution.

**About the data** (link in the top bar): what each source counts, its territory, the period loaded and its licence; the rules the figures follow (no mixing, threshold, sensitive types, no day or hour, partial years); why there is not more data (municipalities under 20,000, no crime by neighbourhood, Mossos and Ertzaintza inside the Ministry's totals, convictions and surveys, citizen contributions); and what the figures cannot say. The periods and dates come from the tables, so the page updates itself.

**Reading the figures**
- Figures below 10 are shown as "<10". No day, hour or sensitive types (homicides, sexual offences, gender-based violence...) are ever shown by area; they only count in totals.
- Rates divide by registered residents: areas with lots of visitors (centre, nightlife, transport, tourism, airports) get high rates that overstate the risk for residents. The app warns when a rate is twice the reference.
- Darker on the map never means "dangerous": it means more calls, incidents or recorded offences per resident (or in total, if shading by number).

## 5. Monthly routine

Madrid publishes each month in the second half of the following month; Barcelona once a year (around February); the Ministry every quarter; Mossos monthly. Run option **1** whenever you want, or schedule it with option **9**. Then open the app.

## 6. When something new appears

The data status (option 4) says when a source has **types without equivalence**. Add them to the matching CSV in `reference/` (one line each, with `block`/`group_id`, `sensitive` and the names in Spanish and English) and run option 1 again: the references are reloaded on every run.

| Source | Reference file |
|---|---|
| Madrid calls | `reference/madrid_call_types.csv` |
| Madrid arrest motives | `reference/madrid_arrest_motives.csv` |
| Crime Balance types | `reference/crime_balance_types.csv` (municipality name aliases in `crime_balance_municipality_aliases.csv`) |
| Mossos offence types / groups / police areas | `reference/mossos_types.csv`, `mossos_groups.csv`, `mossos_abp.csv` |
| Barcelona incident types | `reference/bcn_incident_types.csv` |

Quality reports of every load are in `reports/quality_<source>_*.csv`.

## 7. Files and folders

```
WEBARCHYMAPP.R              the menu
R/                      the code, one file per source (00_config.R has every URL and folder)
app/                    the Shiny app (app.R, i18n.R, translations.csv)
reference/              editable equivalence tables
scripts/                entry points and maintenance scripts
data/raw/country_<cc>/  original downloaded files of each country — never edited, never deleted
data/raw/country_es/    Spain: crime_balance, ine_population, municipal_boundaries, mossos, abp
data/raw/city_madrid/   Madrid: calls, activity, districts, neighbourhoods, census
data/raw/city_barcelona/ Barcelona: incidents, boundaries, census
data/raw/shared/        GISCO, geoBoundaries, Natural Earth, Eurostat files used by several countries

data/webarchymapp.duckdb    the database (rebuildable from data/raw)
data/territory_*.gpkg   official boundaries in WGS84
reports/                quality reports and update logs
```

`scripts/maintenance/remove_old_spanish.R` is the last trace of the move from the Spanish version (November 2026): run it once to delete the old quality reports (`reports/calidad_*.csv`) and the stale `.RData`, move your own copies of the Madrid CSV/PDF out of `Claude/02_datos/`, and it then removes itself. It asks before every step and never touches `data/raw/`.

## 8. Adding a country

Each country is one R file (`R/7x_<cc>.R` or `R/8x_<cc>.R`) that registers itself with `register_country()` and one reference table `reference/<cc>_offence_types.csv` (source category -> offence type, English and local names, parent aggregate, sensitive flag). The file provides four functions: `download()`, `territory(con)`, `ingest(con)` and `population(con)`; the framework in `R/70_countries.R` does the rest (shared tables, marts, status). Use `R/71_fr.R` (France) as the model and `reference/eurostat_regions_countries.csv` to add a European country that only has Eurostat regional data. Rules: official sources with an open licence, no registration, raw files never modified, sensitive offences only inside totals, and never a comparison between countries.

Countries whose module could not be tested against the real files (the first download happens on your computer) print a clear message naming the file and its header when the structure differs from what was documented; run `<cc>_inspect()` (e.g. `fr_inspect()`) to see the headers and adjust the column patterns at the top of the module.

## 9. Adding a language

Add a column to `app/translations.csv` with the language code as header and translate the texts (keep the `%s`/`%d` placeholders and the `|`-separated month lists), then add the code and name to `LANGUAGES` in `app/i18n.R`. Empty cells fall back to English. Names of call types, offence types and arrest motives come from the reference CSVs (Spanish and English only).

## 10. Privacy and interpretation rules built into the project

1. Never day or hour: month × neighbourhood at most.
2. Sensitive types are never shown by area.
3. Only blocks that can be read by area (public safety, nuisance); traffic, ambulance support, 092 closures and the like are left out.
4. Figures below 10 are suppressed after adding up the chosen months.
5. Totals never mix blocks, measures or sources.
6. No rankings of neighbourhoods; the district top 5 always shows rates, raw numbers and the city average, with a note on transient population.
