# Meteorological and Statistical Utilities in Modern Fortran

This repository contains a collection of high-performance kernels and utility scripts written in **Modern Fortran (F95/2003)**. These tools are designed for processing atmospheric data, calculating instability indexes, and classifying climate regimes.

## Atmospheric Physics & Dynamics

- **K-Index (`K_index.f95`)**: Calculates the K-index (thunderstorm potential) using radiosonde data at 850 hPa, 700 hPa, and 500 hPa levels. It includes a classification logic for rain and thunderstorm probability.
- **Koppen Classification (`Koppen_Classification.f95`)**: Implements the Köppen climate classification system based on mean monthly temperature and precipitation data to characterize climate zones (e.g., Af, Bwh, Cfa).
- **Wind Calculations (`Wind.f95`)**: Utilities for calculating wind vectors and components.

## Statistical and Mathematical Kernels

- **Moving Averages (`Central_Moving_Average.f95`)**: Implementation of central moving average filters for time-series smoothing.
- **Correlation & Percentiles (`Correlation_Coefficient.f95`, `Percentiles.f95`)**: High-performance statistical routines for data analysis.
- **Fibonacci & Math Utilities (`Fibonacci.f95`)**: Basic numerical algorithms.

## Geosciences & Time Handling

- **Calendar Utilities (`Calendar.f95`, `Julian_day.f95`)**: Robust routines for converting between Gregorian dates and Julian days, essential for handling long-term climate datasets and model time-steering.

## Compilation

To compile any of the Fortran routines, use a modern Fortran compiler (e.g., `gfortran`):

```bash
gfortran K_index.f95 -o k_index
./k_index
```

## Data Requirements
Some routines (like `K_index.f95`) expect input data in specific formats (e.g., `Data_radiosonde_5_5_2017.txt`). Ensure the data files are present in the same directory as the executable.
