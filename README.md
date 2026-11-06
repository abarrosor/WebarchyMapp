# WebarchyMapp

A personal, non-commercial map of **official crime and police figures** by territory: 47 countries of Europe and the Americas (48 sources: Spain carries the Mossos d'Esquadra as a second one) and 3 cities by neighbourhood (Madrid, Barcelona, Lima and Callao), built with R, DuckDB and Shiny from open data. Every source is shown on its own, with its unit, territory, period and licence; figures of different sources, countries or cities are **never added together or compared**, and the app gives no "safety score".

The code, the reference tables (the project's own equivalence tables) and the documentation are what this repository holds. **The data is not distributed**: the pipeline downloads every file from its official portal under the portal's own terms, and `data/` (raw files, the DuckDB database, the GeoPackages) stays out of the repository.

## How it works

- `WEBARCHYMAPP.R` is a menu: option 1 downloads what is new and rebuilds the tables, option 2 opens the app. `GUIDE.md` explains every option; `Claude/` (one folder up, in the author's workspace) holds the project documentation (decisions, source catalogue, architecture).
- One **module per place**, registered in a shared framework: `R/70_countries.R` (countries, with `register_country()` and `register_source()` for a second source of the same country) and `R/60_cities.R` (cities, with `register_city()`). A module knows only its own portal and files; the framework does the shared tables, the marts, the status and the checks. Adding a country or a city is one R file and one equivalence CSV.
- One DuckDB file. Raw files are never modified; everything can be rebuilt from them. The app reads only the `mart_*` tables, which carry the privacy rules: no day or hour, no sensitive offence types outside totals, a "fewer than 10" threshold applied after adding up the chosen period, measures never mixed.
- Everything is free software and runs locally: no accounts, keys, subscriptions or external services beyond the official portals themselves.

## Requirements

R 4.x with the packages listed in `scripts/maintenance/install_packages.R` (option 5 of the menu installs them). Windows is what the author uses; the code has no Windows dependency except the optional weekly scheduling (Task Scheduler).

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

## Licence of the code

To be chosen before the first public release (MIT or AGPL-3.0; see the project's decision register, P-025). Until then: all rights reserved, personal use.

## Principles

No accusations, no lists of people, no blaming of neighbourhoods: recorded figures depend on what gets reported, on how each police force records it and on how many people are in a place during the day, not only on who lives there. A high figure is a reason to look further, not a verdict on a place.
