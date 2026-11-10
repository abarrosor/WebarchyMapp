# WebarchyMapp — user guide

A personal R/RStudio project that downloads official open data about citizen safety in Spain, loads it into a local DuckDB database and shows it in a Shiny app with maps. Everything runs on your computer; nothing is sent anywhere and nothing costs money.

## 1. What the app shows

| View | Data | Level | Source |
|---|---|---|---|
| **Country › Spain** (the app opens here) | Known criminal offences by region, province and municipality (> 20,000 inhabitants); in Catalonia also by police area (Mossos, a second source of the country) | Quarter / year | Ministry of the Interior (Crime Balance); Government of Catalonia |
| **Country › 46 others** | Recorded offences (or complaints, cases...) by the levels each country publishes, from its official source; the About page lists them all | Quarter / year | National statistics offices, ministries, police forces, Eurostat |
| **Countries (table)** | National rates per 100,000 inhabitants, with the warning that definitions differ between countries | Year | Eurostat |
| **Cities › Madrid** | Calls received by the Municipal Police by neighbourhood; police activity by district; arrests by motive (city) | Month | Madrid City Council (datos.madrid.es) |
| **Cities › Barcelona** | Incidents handled by the City Police by neighbourhood; recorded crime by district (Mossos) | Month | Barcelona City Council (Open Data BCN); Mossos |
| **Cities › Lima and Callao** | Police complaints by distrito (the 50 distritos of Lima Metropolitana and the Callao) | Month | Ministry of the Interior of Peru (SIDPOL) |
| **Cities › Paris** | Recorded crime by arrondissement, 15 SSMSI indicators (yearly figures: the source publishes no months) | Year | Ministère de l'Intérieur, SSMSI (data.gouv.fr) |
| **Cities › Amsterdam** | Crimes registered by the police by wijk and stadsdeel; any other Dutch gemeente can be added the same way | Month | Politie / CBS (table 47022NED) |
| **Cities › London** | Offences recorded by the Metropolitan Police by ward and borough (the City of London is not included) | Month | Greater London Authority, London Datastore |
| **Cities › Mexico City** | Investigation files (carpetas de investigación) of the prosecution service by colonia and alcaldía | Month | Fiscalía General de Justicia de la CDMX (datos.cdmx.gob.mx) |

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
| 8 | **Update only the cities** (every city module: Madrid, Barcelona, Lima and Callao, Paris, Amsterdam, London, Mexico City, with their panels and surveys); option 1 includes them |
| 9 | Schedule the automatic weekly update (Windows Task Scheduler; runs `Rscript` without opening RStudio) |
| 10 | Remove the automatic update |
| 11 | Check whether the update is scheduled and how the last run went (`reports/update_*.log`) |
| 12 | Rebuild the database from scratch from `data/raw/` (originals untouched) |
| 13 | **Update every country** (the *Country* menu of the app, Spain included) and the Eurostat tables; option 1 includes them. A country whose raw and reference files did not change since its last load is kept as it is (Canada alone parses 50 GB); to reload one after a code fix: option 14, or `update_countries(only = "CA")` in the console |
| 14 | **Reload some countries or cities**: asks for the codes (`GBE GBS FI`, separated by spaces, no quotes) and reloads them even if their files did not change. Use it after a fix to one module. Spain is `ES` (Crime Balance, INE population, IGN boundaries, and its second source, the Mossos d'Esquadra by police area); the cities are `MAD` (Madrid), `BCN` (Barcelona) and `LIM` (Lima and Callao) |
| 15 | **Check the consistency of the data** (cities, then countries and their second sources): duplicates, territories without name, cumulative figures that decrease, parts above the total, population coverage, polygons without territory, quarters in the future. Prints the findings and saves `reports/country_checks_<date>.csv`. Read-only |

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
| Crime Balance types (Spain) | `reference/es_offence_types.csv` (`source_code` is the normalised key of the published label, `source_text` the label; municipality name aliases in `es_municipality_aliases.csv`, provinces and regions in `es_provinces.csv`, URLs in `es_sources.csv`) |
| Mossos offence types / groups / police areas | `reference/mossos_types.csv`, `mossos_groups.csv`, `mossos_abp.csv` |
| Barcelona incident types | `reference/bcn_incident_types.csv` |

Quality reports of every load are in `reports/quality_<source>_*.csv`.

## 7. Files and folders

```
WEBARCHYMAPP.R              the menu
R/                      the code: the two frameworks (60_cities.R, 70_countries.R), one file per city module
                        (61_city_madrid, 62_city_barcelona, 63_city_lima, 64_city_paris, 65_city_nl (Amsterdam and any
                        gemeente), 66_city_london, 67_city_cdmx) and per country module (71_es.R is Spain,
                        72_es_mossos.R its second source; 7x, 8x, 9x the others), the readers the Spanish modules
                        use (10_, 11_, 20_, 21_, 22_, 30_, 31_), the marts (50_), the pipeline (60_pipeline.R),
                        the checks (94_); 00_config.R has the folders, reference/<cc>_sources.csv the URLs
app/                    the Shiny app (app.R, i18n.R, translations.csv)
reference/              editable equivalence tables
scripts/                entry points and maintenance scripts
data/raw/country_<cc>/  original downloaded files of each country — never edited, never deleted
data/raw/country_es/    Spain: crime_balance, ine_population, municipal_boundaries, mossos, abp
data/raw/city_madrid/   Madrid: calls, activity, districts, neighbourhoods, census
data/raw/city_barcelona/ Barcelona: incidents, boundaries, census, survey (victimisation survey by district, CSV
                        transcribed from the yearly report or exported from the Portal de Dades; see its README.txt)
                        (Lima and Callao use the Peru files; Paris the France communal file plus city_paris/boundaries)
data/raw/city_amsterdam/ Amsterdam: OData pages of table 47022NED by year, wijken GeoJSON, gebieden (stadsdelen) files
data/raw/city_london/   London: crime (ward CSVs), boundaries (ONS ward chunks), population (a ward population file you add)
data/raw/city_cdmx/     Mexico City: carpetas (the cumulative FGJ file, hundreds of MB, one copy per month), colonias
data/raw/shared/        GISCO, geoBoundaries, Natural Earth, Eurostat files used by several countries

data/webarchymapp.duckdb    the database (rebuildable from data/raw)
data/territory_*.gpkg   official boundaries in WGS84
reports/                quality reports and update logs
```


## 8. Adding a country

Each country, Spain included (`R/71_es.R`), is one R file (`R/7x_<cc>.R`, `R/8x_<cc>.R`, `R/9x_<cc>.R`) that registers itself with `register_country()` and one reference table `reference/<cc>_offence_types.csv` (source category -> offence type, English and local names, parent aggregate, sensitive flag). The file provides four functions: `download()`, `territory(con)`, `ingest(con)` and `population(con)`; the framework in `R/70_countries.R` does the rest (shared tables, marts, status). A country may carry a **second source** (a regional police, say): a file loaded after the country's that calls `register_source(cc, list(id, name, levels, level_labels, covers, ...))` with the same four functions and its own `reference/<cc>_<id>_offence_types.csv`; the app shows it as a switch of source inside the territory it covers (`covers` = a region id or the country code) and never adds it to the first one. `R/72_es_mossos.R` is the model. Use `R/71_fr.R` (France) as the model and `reference/eurostat_regions_countries.csv` to add a European country that only has Eurostat regional data. Rules: official sources with an open licence, no registration, raw files never modified, sensitive offences only inside totals, and never a comparison between countries.

Countries whose module could not be tested against the real files (the first download happens on your computer) print a clear message naming the file and its header when the structure differs from what was documented; run `<cc>_inspect()` (e.g. `fr_inspect()`) to see the headers and adjust the column patterns at the top of the module.

## 8b. Adding a city

A city is one R file (`R/6x_city_<key>.R`) that registers itself with `register_city()` (code, key, municipality id, names, language, country, source, and the four functions `download()`, `territory(con)`, `ingest(con)`, `population(con)`). The territory step writes the districts and neighbourhoods into `dim_territory` and `data/territory_<key>.gpkg` (layer `territory`); the ingest step calls `publish_city_events()` with a view of monthly events by official neighbourhood and a type table (`type, block, sensitive, name_es, name_en`); the population step fills `ref_population` with the city register by neighbourhood. The framework (`R/60_cities.R`) builds `mart_city_events`, `mart_city_event_types` and `mart_city_meta`, and the app shows the city with the same map, filters and profile as Madrid and Barcelona. City-specific texts go in `translations.csv` as `key@<key>` variants. A city may also carry a **survey** (victimisation, perception of security): `register_city_survey(key, list(id, name, publisher, url, licence, measure, ingest))`, whose `ingest(con)` calls `publish_city_survey()` with yearly values by city or district and the indicator definitions; the app shows it in the profile as a measure of its own (Barcelona's Enquesta de Victimització is the model, at the end of `R/62_city_barcelona.R`). Use `R/62_city_barcelona.R` as the model, `R/63_city_lima.R` for a city built on a country module's own data (it runs after the countries: `after_countries = TRUE`), `R/64_city_paris.R` for a **yearly source** (`time_unit = "year"`: the app shows years instead of months and no seasonal profile), `R/65_city_nl.R` for a **generator** that describes many cities of one source (`register_city(nl_city_module(code, key, gm, name, id))`: one line per Dutch gemeente), `R/66_city_london.R` and `R/67_city_cdmx.R` for sources read from CSV files with their own portals. Module fields that change the texts: `measure_kind` ("calls", "incidents", "complaints", "offences", "cases") picks the set of texts the app uses when a key has no `key@<city>` variant (`key@offences`...), and `boundaries_attribution` is the credit under the map when `translations.csv` has no `boundaries_source@<key>`. A city may also carry **panels** (a second monthly dataset by district or for the whole city: police activity, arrests by motive, a regional police by district): `register_city_panel(key, list(id, kind = "district" | "city", name, publisher, url, licence, measure, ingest, ...))`, whose `ingest(con)` calls `publish_city_panel()` with monthly values by territory and indicator plus the indicator table; the app shows a "district" panel in the neighbourhood profile and a "city" panel in the statistics tab, with the translation keys the panel names or generic texts. The panels of Madrid (`R/61_city_madrid.R`) and Barcelona (`R/62_city_barcelona.R`) are the models.

Cities whose module was written without the real files (Paris, Amsterdam, London, Mexico City) print a clear message when a file does not match what was documented: `nl_city_inspect("amsterdam")`, `london_inspect()` and `cdmx_inspect()` show what was downloaded and the headers; the URLs and dataset ids live in `reference/london_sources.csv` and `reference/cdmx_sources.csv`, the type equivalences in `reference/nl_city_event_types.csv` and `reference/cdmx_event_types.csv`. London has no population in its source: put a ward population file (ONS mid-year estimates or Census 2021, any CSV/XLSX with a ward code column and a total) in `data/raw/city_london/population/` to get rates. Mexico City's colonias have no official population: rates exist for the alcaldías and the city only.

## 9. Adding a language

Add a column to `app/translations.csv` with the language code as header and translate the texts (keep the `%s`/`%d` placeholders and the `|`-separated month lists), then add the code and name to `LANGUAGES` in `app/i18n.R`. Empty cells fall back to English. Names of call types, offence types and arrest motives come from the reference CSVs (Spanish and English only).

## 10. Privacy and interpretation rules built into the project

1. Never day or hour: month × neighbourhood at most.
2. Sensitive types are never shown by area.
3. Only blocks that can be read by area (public safety, nuisance); traffic, ambulance support, 092 closures and the like are left out.
4. Figures below 10 are suppressed after adding up the chosen months.
5. Totals never mix blocks, measures or sources.
6. No rankings of neighbourhoods; the district top 5 always shows rates, raw numbers and the city average, with a note on transient population.

## 11. Known problems

- **"LoadLibrary failure: Una directiva de Control de aplicaciones bloqueó este archivo"** (or the English "An Application Control policy has blocked this file") when a package loads, e.g. `classInt.dll`. Windows 11's Smart App Control checks the reputation of every DLL and blocks one it does not know; R packages are not signed, so a freshly updated package can be blocked while the rest load. Fix: stop the app, type `install.packages("<the package named>")` in the console (straight quotes), restart R and try again: the current CRAN binary usually has reputation. Seen 2026-11-09 with `classInt`; solved that way. If the new file is blocked too, the options are turning Smart App Control off (Windows Security → App & browser control; irreversible) or running R under WSL2.

## 12. Licence

The code and the reference tables are under the MIT License (file `LICENSE` in the project folder; decision P-034): anyone may use, copy, change and redistribute them, keeping the copyright notice; nothing is owed to the author, no warranty is given. The data is never distributed: `data/` stays out of the repository and every figure keeps the terms of the portal it came from (About the data). When publishing the repository, check that `.gitignore` still lists `data/`, `reports/` and `_old_spanish/`, and put your name or GitHub user in the copyright line of `LICENSE`.
