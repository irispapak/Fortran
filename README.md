# Fortran Utilities

This folder contains small Fortran programs for meteorology, basic statistics, and time handling. The structure is:

- `src/` Fortran source files (`.f95`)
- `data/` sample input files used by some programs
- `output/` sample outputs and default output locations

## Program Guide

| Program | Purpose | Inputs | Outputs |
| --- | --- | --- | --- |
| `src/Calendar.f95` | Builds a day-by-day calendar between a user-provided start and end year. | User input (start year, end year). | `output/calendar.txt` (day, month, year per line). |
| `src/Julian_day.f95` | Converts a user-provided date to Julian day-of-year. | User input (year, month, day). | Printed to stdout. |
| `src/Fibonacci.f95` | Generates a Fibonacci sequence of length `N`. | User input (sequence length). | Printed to stdout. |
| `src/Central_Moving_Average.f95` | Computes central moving averages (3, 5, 7, 9 points) for each column in a time series. | `data/fileB.prn`. | `output/cma1.txt`, `output/cma2.txt`, `output/cma3.txt`, `output/cma4.txt`. |
| `src/Correlation_Coefficient.f95` | Correlation coefficient between column 1 and columns 2-4. | `data/fileB.prn`. | Printed to stdout. |
| `src/Percentiles.f95` | 90th percentile of max temperature and 10th percentile of min temperature. | `data/Thessaloniki.txt`. | Printed to stdout. |
| `src/Wind.f95` | Computes wind speed and direction from u/v components. | `data/TableA.txt`. | `output/wind.txt`. |
| `src/K_index.f95` | Computes the K-index for thunderstorm potential and classifies risk. | `data/Data_radiosonde_5_5_2017.txt`. | Printed to stdout. |
| `src/Koppen_Classification.f95` | Assigns a Koppen climate class from monthly temp/precip. | User input (station name, 12 temps, 12 precip values). | Printed to stdout. |

## How To Compile

Run from the `Fortran/` directory so the relative paths to `data/` and `output/` work:

```bash
gfortran src/K_index.f95 -o k_index
./k_index
```

## Notes

- Programs that read input files assume the file formats used in `data/`.
- Programs that write files place outputs in `output/` by default.
