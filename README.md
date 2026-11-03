# WebarchyMapp

Personal citizen-safety map of Spain built with R, DuckDB and Shiny from official open data. See **GUIDE.md** for how to use it and `Claude/` (one folder up) for the project documentation (decisions, source catalogue, architecture).

- Start: open `WebarchyMapp.Rproj`, source `WEBARCHYMAPP.R`, choose option 1, then 2.
- Sources: Madrid City Council (calls F1, police activity F2, register, boundaries), Spanish Ministry of the Interior (Crime Balance F3), INE (population), IGN (boundaries via mapSpain), Government of Catalonia (Mossos F4, police areas), Barcelona City Council (City Police incidents F5, register, boundaries).
- Everything is free software and runs locally. No accounts, keys or subscriptions.
- The app reads only the `mart_*` tables, which carry the privacy rules (no day/hour, no sensitive types, threshold of 10, measures never mixed).
