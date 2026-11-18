# WebarchyMapp

Code under the [MIT License](LICENSE), © 2026 Adrian Barroso Ricote (abarrosor) · data not distributed (see below) · personal, non-commercial project

A personal, non-commercial map of **official crime and police figures** by territory: 47 countries of Europe and the Americas (48 sources: Spain carries the Mossos d'Esquadra as a second one) and 17 cities by neighbourhood (Madrid, Barcelona, Lima and Callao, Paris, Lyon, Marseille, Amsterdam, Rotterdam, Den Haag, Utrecht, Eindhoven, London, Mexico City, Berlin, Buenos Aires, Zürich, Toronto), built with R, DuckDB and Shiny from open data. Every source is shown on its own, with its unit, territory, period and licence; figures of different sources, countries or cities are **never added together or compared**, and the app gives no "safety score".

The code, the reference tables (the project's own equivalence tables) and the documentation are what this repository holds. **The data is not distributed**: the pipeline downloads every file from its official portal under the portal's own terms, and `data/` (raw files, the DuckDB database, the GeoPackages) stays out of the repository. So do the reports and the one table transcribed by hand (`data_seed/`). Before every commit, menu option 19 checks every file that would be published (`R/96_git_prepare.R`; `GUIDE.md`, section 14).

## How it works

- `WEBARCHYMAPP.R` is a menu: option 1 downloads what is new and rebuilds the tables, option 2 opens the app. `GUIDE.md` explains every option; `Claude/` (one folder up, in the author's workspace) holds the project documentation (decisions, source catalogue, architecture).
- One **module per place**, registered in a shared framework: `R/70_countries.R` (countries, with `register_country()` and `register_source()` for a second source of the same country) and `R/60_cities.R` (cities, with `register_city()`). A module knows only its own portal and files; the framework does the shared tables, the marts, the status and the checks. Adding a country or a city is one R file and one equivalence CSV.
- One DuckDB file. Raw files are never modified; everything can be rebuilt from them. The app reads only the `mart_*` tables, which carry the privacy rules: no day or hour, no sensitive offence types outside totals, a "fewer than 10" threshold applied after adding up the chosen period, measures never mixed.
- **A rate stands on the population of its own year**: the figures of 2024 on the inhabitants of 2024, a range of years on the average of its years. Where a year has no population figure, the nearest one is used and the app marks it with a † and a note naming both years; the inhabitants and their year are written beside every rate, and the rate charts say which way they went.
- The app (`app/`, Shiny + Leaflet) opens on Spain and has a view per country and per city with the same controls: map by level, offence types, period, a profile of the clicked territory with its rate compared with the reference (location quotient, percentile band) and with the territories that border it (same source, types and period; of the same country or city only), a box on the map that says where the map is, a statistics tab, a time-series block (monthly / quarterly / yearly, with a period bar of two handles; by number and, where there is a population, by rate, both over the same bar) an About page built from the tables, a "Cite this data" link that gives the citation of the original source (never the app as a publisher), and, on a city's statistics page, a button that hands over the figures on screen as an Excel file whose first sheet carries that citation. Every table can be put in the order of any of its columns by a click on the title of the column. Thirty-two languages (`app/translations.csv`: at least one official language of every country with data; written by the assistant, not by native speakers), a phone layout, every place loaded the first time its view opens.
- **An update says at its end what did not go as planned** (`R/05_journal.R`): the steps that failed, the portals that did not answer, the warnings and the modules' own lines of trouble, under the place that raised them, compared with the last run of that place (new, no longer happening, the same). A source whose last period is older than its rhythm allows is marked; a country whose reader changed is loaded again by itself; the versions of R and the packages are written down when they change. Nothing of this changes what an update does: it listens.
- Everything is free software and runs locally: no accounts, keys, subscriptions or external services beyond the official portals themselves.
- The app opens at a fixed local address (`http://127.0.0.1:7175`) in the light or the dark look of the reader's system; a button at the top left turns it round. The system's look is asked in the reader's browser and kept nowhere in the project. In the dark look a map is a dark map: light and dark are exchanged for the whole of it, legend included, and its help texts say *a stronger colour*, which is true of both looks.
- `tests/run_tests.R` (menu option 16) runs a `testthat` suite on a temporary copy of the code with synthetic files (a synthetic country; Zürich, Toronto, Buenos Aires and Utrecht in the layouts their modules expect): the helpers, both frameworks, the editable tables, the app and the summary of an update (about 2,000 checks), in two minutes or so, without touching the data folder.

## Requirements

R 4.3 or newer with the packages listed in `R/01_utils.R` (`PACKAGES`; option 5 of the menu installs them, `scripts/maintenance/install_packages.R` does the same from a console). Optional: `readODS` (England and Wales tables), `writexl` (the Excel file of a city's statistics page: without it the page has no button; also used by the tests) and `testthat` (the tests; option 16 installs both). Windows is what the author uses; the code has no Windows dependency except the optional weekly scheduling (Task Scheduler).

## Data sources and licences

Read from each module's metadata (`register_country()` / `register_city()`); the licence is the one stated by the portal at the time of writing, check it before reusing anything. Boundaries: national mapping agencies when open, Eurostat GISCO (© EuroGeographics, non-commercial use), geoBoundaries (CC BY 4.0), OCHA COD (CC BY-IGO) or Natural Earth (public domain). Sources that require registration, forbid redistribution or allow personal use only (UNODC, GADM, Belgium, Sweden, Honduras...) are not loaded.

### Countries

| Country | Source | Publisher | Licence | Portal |
|---|---|---|---|---|
| Spain | Crime Balance (Balance de Criminalidad) | Ministry of the Interior, Statistical Crime Portal | Open reuse citing the Statistical Crime Portal (Law 37/2007) | estadisticasdecriminalidad.ses.mir.es |
| Spain (mossos) | Mossos d'Esquadra: offences known by basic police area | Government of Catalonia, Department of the Interior (open data) | Open data of the Government of Catalonia (see the terms of use of the portal) | analisi.transparenciacatalunya.cat |
| France | Bases statistiques de la délinquance enregistrée (SSMSI) | Ministère de l'Intérieur, SSMSI | Licence Ouverte 2.0 | www.data.gouv.fr |
| Netherlands | Geregistreerde criminaliteit; soort misdrijf, regio (StatLine 83648NED) | Centraal Bureau voor de Statistiek (CBS), from Politie data | CC BY 4.0 | opendata.cbs.nl |
| Portugal | Crimes registados pelas autoridades policiais (INE / DGPJ) | INE - Instituto Nacional de Estatística, data from DGPJ (Ministério da Justiça) | Creative Commons Attribution 4.0 (CC BY 4.0) | www.ine.pt |
| England and Wales | Police recorded crime open data tables (Community Safety Partnerships) | Home Office | Open Government Licence v3.0 | www.gov.uk |
| Scotland | Recorded Crimes and Offences | Scottish Government (Police Scotland data) | Open Government Licence v3.0 | data.gov.scot |
| Northern Ireland | Police Recorded Crime Monthly Data | Police Service of Northern Ireland (OpenDataNI) | Open Government Licence (OGL) | admin.opendatani.gov.uk |
| Denmark | Anmeldte forbrydelser (StatBank STRAF11) | Danmarks Statistik (Statistics Denmark) | CC BY 4.0 | www.statbank.dk |
| Finland | Offences recorded by ICCS and municipality (StatFin 13kq) | Tilastokeskus (Statistics Finland) | CC BY 4.0 | pxdata.stat.fi |
| Mexico | Incidencia delictiva del fuero común, nueva metodología (SESNSP) | Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP) | Términos de Libre Uso de la Información del Gobierno de México (Libre Uso MX) | www.gob.mx |
| Colombia | Estadística delictiva de la Policía Nacional (SIEDCO) - datasets abiertos | Policía Nacional de Colombia, DIJIN (via datos.gov.co, MinTIC) | Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0) | www.datos.gov.co |
| Canada | Incident-based crime statistics, by detailed violations (Uniform Crime Reporting Survey), tables 35-10-0177-01 to 35-10-0185-01 | Statistics Canada, Canadian Centre for Justice and Community Safety Statistics | Statistics Canada Open Licence | www150.statcan.gc.ca |
| Costa Rica | Estadísticas Policiales (OIJ) | Poder Judicial de Costa Rica, Organismo de Investigación Judicial | Creative Commons Attribution (CC BY) | datosabiertospj.poder-judicial.go.cr |
| Brazil | SINESP VDE – base de dados dos gestores estaduais | Ministério da Justiça e Segurança Pública (MJSP), SENASP | No licence stated on the dataset page (gov.br content: CC BY-ND 3.0 BR); public data, no registration | www.gov.br |
| Argentina | SNIC – Estadísticas criminales por provincias y departamentos | Ministerio de Seguridad Nacional, Dirección Nacional de Estadística Criminal | Creative Commons Attribution 4.0 (datos.gob.ar) | datos.gob.ar |
| Uruguay | Delitos denunciados en el Uruguay (Ministerio del Interior) | Ministerio del Interior, Observatorio de Violencia y Criminalidad | Licencia de Datos Abiertos del Gobierno de Uruguay (Licencia DAG) | catalogodatos.gub.uy |
| Chile | CEAD – Estadísticas delictuales (casos policiales) | Centro de Estudios y Análisis del Delito (CEAD), Ministerio de Seguridad Pública | No licence stated (public statistics, free access without registration); table exported by hand from the CEAD tool | cead.minsegpublica.gob.cl |
| Germany | Polizeiliche Kriminalstatistik (PKS), BKA | Bundeskriminalamt (BKA) | Datenlizenz Deutschland – Namensnennung – Version 2.0 (attribution: PKS Bundeskriminalamt, Berichtsjahr, Version) | www.bka.de |
| Switzerland | Polizeiliche Kriminalstatistik (PKS), BFS | Bundesamt für Statistik (BFS) | OPEN-BY (BFS: open use, source must be stated) | www.pxweb.bfs.admin.ch |
| Poland | Bank Danych Lokalnych - przestępstwa stwierdzone przez Policję (GUS) | Główny Urząd Statystyczny (GUS) | CC BY 4.0 | bdl.stat.gov.pl |
| Czechia | Kriminalita - data za rok (ČSÚ, from Police Presidium ESSK) | Český statistický úřad (ČSÚ) | CC BY 4.0 (ČSÚ terms of use) | csu.gov.cz |
| Hungary | STADAT 11.1.2.1 Regisztrált bűncselekmények vármegye és régió szerint (KSH) | Központi Statisztikai Hivatal (KSH), from BM / Legfőbb Ügyészség (ENyÜBS) | KSH terms of use: free re-use with the attribution 'Forrás: KSH' (not a Creative Commons licence) | www.ksh.hu |
| Ireland | CJQ10 Recorded Crime Incidents by Garda Division (CSO PxStat) | Central Statistics Office (CSO), from An Garda Síochána PULSE | CC BY 4.0 | data.cso.ie |
| Austria | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Belgium | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Bulgaria | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Croatia | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Cyprus | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Estonia | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Greece | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Italy | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Latvia | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Lithuania | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Luxembourg | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Malta | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Romania | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Slovakia | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Slovenia | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Sweden | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Norway | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Iceland | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Türkiye | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Serbia | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Montenegro | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Albania | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Liechtenstein | Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region) | Eurostat, from the national authorities | CC BY 4.0 (Eurostat) | ec.europa.eu |
| Peru | Denuncias Policiales (SIDPOL) | Ministerio del Interior (MININTER) / Policía Nacional del Perú, Plataforma Nacional de Datos Abiertos | Open Data Commons Attribution License (ODC-BY) | www.datosabiertos.gob.pe |

### Cities

| City | Source | Publisher | Licence | Portal |
|---|---|---|---|---|
| Madrid | Calls received by the Municipal Police central dispatch | Madrid City Council - Municipal Police | CC BY 4.0 | datos.madrid.es |
| Barcelona | Incidents handled by the Guardia Urbana in the city of Barcelona | Barcelona City Council - Guardia Urbana | CC BY 4.0 | opendata-ajuntament.barcelona.cat |
| Lima and Callao | Police complaints (SIDPOL) by distrito | Ministerio del Interior (MININTER) / Policía Nacional del Perú, Plataforma Nacional de Datos Abiertos | Open Data Commons Attribution License (ODC-BY) | www.datosabiertos.gob.pe |
| Paris, Lyon, Marseille | Recorded crime by arrondissement (SSMSI communal base), yearly | Ministère de l'Intérieur, SSMSI (data.gouv.fr); boundaries and legal population geo.api.gouv.fr (Etalab) | Licence Ouverte 2.0 | www.data.gouv.fr |
| Amsterdam, Rotterdam, Den Haag, Utrecht, Eindhoven | Registered crimes by wijk, monthly (table 47022NED) | Politie, through Centraal Bureau voor de Statistiek (dataderden.cbs.nl); boundaries and residents CBS Wijk- en buurtkaart; Amsterdam's stadsdelen from the city's gebieden API | CC BY 4.0 | dataderden.cbs.nl |
| London | Metropolitan Police recorded crime by ward (London Datastore) | Greater London Authority / Metropolitan Police Service; boundaries ONS Open Geography; residents ONS mid-year ward estimates | Open Government Licence | data.london.gov.uk |
| Mexico City | Investigation files of the FGJ (carpetas de investigación) by colonia | Fiscalía General de Justicia de la Ciudad de México, Portal de Datos Abiertos | Open data of Mexico City (see the terms of the portal) | datos.cdmx.gob.mx |
| Berlin | Kriminalitätsatlas Berlin by Bezirksregion, yearly | Polizei Berlin, Berlin Open Data; boundaries Geoportal Berlin (LOR, CC BY 3.0 DE); residents implied by the atlas's frequency figures | CC BY-SA | daten.berlin.de |
| Buenos Aires | Reported offences by barrio and comuna, monthly | Gobierno de la Ciudad de Buenos Aires, Buenos Aires Data; residents by comuna INDEC, Censo 2022 | Creative Commons Attribution | data.buenosaires.gob.ar |
| Zürich | Offences of the Criminal Code by Stadtkreis, yearly (PKS) | Kantonspolizei Zürich, open data of the Kanton Zürich (residents in the file); boundaries Stadt Zürich open data | Open use with attribution | zh.ch |
| Toronto | Reported major crimes by neighbourhood, yearly (Neighbourhood Crime Rates) | Toronto Police Service, Public Safety Data Portal | Open Government Licence – Ontario | data.tps.ca |

Any other Dutch gemeente is one line in `R/65_city_nl.R` (`register_city(nl_city_module(...))`), and any other French city with arrondissements one line in `R/64_city_fr.R`. Madrid and Barcelona also carry extra panels (Municipal Police activity and arrests by motive; Mossos d'Esquadra by district) and Barcelona a victimisation survey, all declared by their modules through the shared panel and survey contracts.

## Licence

The code, the reference tables in `reference/` (the project's own equivalence tables) and the documents of this repository are released under the **MIT License** (`LICENSE`): use, copy, modify and redistribute freely, for any purpose, keeping the copyright notice and the licence text. No warranty of any kind. If you build something on it, a note or an issue saying so is welcome, though not required.

The licence covers the project's own work only. **It does not cover the data**: every figure the app shows is downloaded by the pipeline from its official portal and stays under that portal's own terms (table above); nothing in `data/` is part of the repository.

The licence does not lift the project's principles either: the code refuses to add or compare different measures, suppresses small figures and keeps sensitive offences inside totals. Anyone may change that in a copy (MIT allows it), but a copy that does so is no longer WebarchyMapp and should not be presented as such.

## Principles

No accusations, no lists of people, no blaming of neighbourhoods: recorded figures depend on what gets reported, on how each police force records it and on how many people are in a place during the day, not only on who lives there. A high figure is a reason to look further, not a verdict on a place.
