# Emisiones México 2008

Sistema para convertir emisiones anuales a formato WRF-Chem (año base 2008).

[![bash](https://img.shields.io/badge/bash-%E2%89%A54.0-blue?logo=gnu-bash)](#construcción)
[![Language: Fortran](https://img.shields.io/badge/Language-Fortran%2090-orange.svg)]()
[![Institution](https://img.shields.io/badge/Institution-CCA%20UNAM-red.svg)](https://www.atmosfera.unam.mx/)
[![WRF-Chem](https://img.shields.io/badge/Model-WRF--Chem-lightblue.svg)](https://ruc.noaa.gov/wrf/wrf-chem/)

## Descripción

Inventario de emisiones para modelación de calidad del aire con **WRF-Chem** para la región
**Nacional** (año base 2008). Incluye emisiones de contaminantes criterio (CO, NO₃, SO₂,
PM₂.₅, PM₁₀, COV) organizadas en sectores: fuentes móviles, fuentes de área, fuentes de
punto y biogénicas.

El sistema de compilación está basado en **GNU Autotools**, que reemplaza al script
`abril_2016.csh` original (J. A. García Reynoso, 26/07/12).

---

## Estructura del repositorio

```
mexico_2008/
├── 01_pob/          # Población y malla: celda_pob, crea_grib
├── 02_aemis/        # Distribución espacial de las emisiones de área
├── 03_movilspatial/ # Agrupa la malla de emisiones por vialidades y carreteras
├── 04_temis/        # Distribución temporal de las emisiones de área (anual → horaria)
├── 05_semisM/       # Distribución espacial de emisiones de fuentes móviles
├── 06_temisM/       # Distribución temporal de las emisiones de fuentes móviles
├── 07_puntual/      # Distribución temporal de las emisiones de fuentes fijas
├── 08_spec/         # Especiación de COV según el mecanismo químico a usar
├── 09_pm25spec/     # Especiación de PM2.5 en especies inorgánicas y orgánicas
├── 10_storage/      # Lee salidas anteriores y genera archivo NetCDF para WRF-Chem
├── 12_biogenic/     # Emisiones biogénicas (espacial y temporal)
├── 13_nonroad/      # Datos de emisiones fuera de camino (solo datos)
├── README.md
└── .gitignore
```

---

## Requisitos del sistema

- Fortran 90/95 o superior (`gfortran` ≥ 9, `ifort`, `ifx`, o `flang`)
- Bibliotecas **NetCDF-Fortran** (`libnetcdf`, `libnetcdff`)
- **GNU Autotools** (`autoconf`, `automake`) para la compilación
- WRF-Chem (para usar las emisiones generadas)

---

## Construcción

### Compilación rápida

```bash
./autogen.sh          # solo la primera vez o si cambia configure.ac
./configure
make -j8
make install          # opcional
```

Con Intel y NetCDF (equivalente al script original):

```bash
export NETCDF=/opt/netcdf
./configure FC=ifort
make -j8
```

Con **gfortran en macOS** (Homebrew):

```bash
brew install gcc netcdf netcdf-fortran automake autoconf
./autogen.sh
./configure FC=gfortran-14 --with-netcdf=$(brew --prefix netcdf-fortran)
make -j8
```

### Opciones de `configure`

| Opción | Efecto |
|---|---|
| `FC=ifort` \| `FC=ifx` \| `FC=gfortran` | Elige compilador (se busca en ese orden) |
| `--with-netcdf=DIR` | Raíz de NetCDF-Fortran; por omisión usa `$NETCDF` o `nf-config` |
| `--enable-optimization=N` | Nivel `-ON`, por omisión 3 |
| `--disable-avx` | No usar banderas de arquitectura nativa (`-axAVX`, `-march=native`, `-mcpu=native`) |
| `--disable-varfmt` | Forzar `-DPGI` aunque el compilador admita `format(<n>...)` |
| `--enable-debug` | `-O0 -g` con verificaciones y traceback |
| `--disable-exe-suffix` | Ejecutables sin la extensión `.exe` |
| `FCFLAGS="..."` | Si se define, sustituye a las banderas automáticas |

Si no se encuentra NetCDF-Fortran, `configure` avisa y omite el directorio `10_storage`;
los demás programas se compilan igual.

### macOS y Apple Silicon

`configure` detecta la arquitectura (`AC_CANONICAL_HOST`) y elige la bandera que corresponde:

| Equipo | Bandera |
|---|---|
| Apple Silicon (arm64) | `-mcpu=native` (respaldo: `-mcpu=apple-m1`, `-mtune=native`) |
| x86_64 con Intel `ifort`/`ifx` | `-axAVX` |
| x86_64 con GNU u otros | `-march=native` (respaldo: `-mavx`) |

### Preprocesador y `format(<n>...)`

Las fuentes `.f90` (minúscula) no se preprocesan por omisión. Como el código usa `#ifndef PGI`,
`configure` detecta la bandera adecuada (`-cpp` en GNU, `-fpp` en Intel, `-Mpreprocess` en NVIDIA)
y la aplica a todas las fuentes.

`gfortran`, NVIDIA/PGI y `flang` no admiten expresiones de formato variables tipo `format(I3,<n>(...))`,
extensión de Intel/Cray. En ese caso se compila con `-DPGI` y el código usa la rama de repetición fija.

---

## Programas generados

| Directorio | Fuente | Ejecutable |
|---|---|---|
| `01_pob` | `celda_pob.f90` | `cpob.exe` |
| `01_pob` | `celda_pob2.f90` | `cpob2.exe` |
| `01_pob` | `crea_gri_pb.f90` | `cgrib.exe` |
| `01_pob` | `crea_grib.f90` | `cgrib2.exe` |
| `02_aemis` | `area_espacial.f90` | `ASpatial.exe` |
| `02_aemis` | `genera_covertura.f90` | `covertura.exe` |
| `03_movilspatial` | `suma_carretera.f90` | `carr.exe` |
| `03_movilspatial` | `suma_vialidades.f90` | `vial.exe` |
| `03_movilspatial` | `agrega.f90` | `agrega.exe` |
| `03_movilspatial` | `crea_fcft.f90` | `fcft.exe` |
| `04_temis` | `atemporal.f90` | `Atemporal.exe` |
| `05_semisM` | `movil_spatial.f90` | `MSpatial.exe` |
| `05_semisM` | `fuera_camino.f90` | `fuera.exe` |
| `05_semisM` | `sumamovil.f90` | `sumamovil.exe` |
| `06_temisM` | `movil_temp.f90` | `Mtemporal.exe` |
| `07_puntual` | `t_puntal.f90` | `Puntual.exe` |
| `08_spec` | `agg_a.f90`, `agg_m.f90`, `agg_p.f90` | `spa.exe`, `spm.exe`, `spp.exe` |
| `09_pm25spec` | `pm25_speci_{a,m,p}.f90` | `spm25{a,m,p}.exe` |
| `10_storage` | `guarda2_nc2008.f90` | `radm2.exe` |
| `12_biogenic` | `s_biogenic.f90` | `Sbiogenic.exe` |
| `12_biogenic` | `btemporal.f90` | `Btemporal.exe` |

---

## Uso

1. Preparar los datos de entrada en los directorios correspondientes
2. Editar las variables `mes` y `dia` dentro de **`ejecuta.sh.in`** si se desea otra fecha
3. Compilar con `make -j8` (ver sección [Construcción](#construcción))
4. Ejecutar la cadena completa:

```bash
./ejecuta.sh      # o bien:  make run
```

### Orden de etapas y paralelismo

| Etapa | Directorio(s) | Descripción | Depende de |
|------:|--------------|-------------|------------|
| 1 | `02_aemis` | Distribución espacial de área | — |
| 2 | `04_temis`, `07_puntual`, `06_temisM` | Temporal área + Puntual + Temporal móviles *(paralelo)* | 1 |
| 3 | `03_movilspatial` | Carreteras + vialidades *(paralelo)* | — |
| 4 | `03_movilspatial` | Agregación de móviles | 3 |
| 5 | `05_semisM` | Distribución espacial de móviles | 4 |
| 6 | `08_spec`, `09_pm25spec` | Especiación VOC + PM₂.₅ *(paralelo)* | 2, 5 |
| 7 | `10_storage` | Generación NetCDF para WRF-Chem | 6 |

### Log de tiempos (`ejecuta.log`)

Al finalizar, `ejecuta.log` incluye un resumen de tiempos por etapa y proceso.
La salida final (archivo NetCDF para WRF-Chem) se guarda en **`10_storage/`**.

### Otros blancos de `make`

```bash
make clean        # borra objetos, módulos y ejecutables
make distclean    # además borra Makefiles y config.status
make dist         # tarball distribuible
make distcheck    # verifica que el tarball compila desde cero
make -C 08_spec   # compila un solo directorio
```

> La compilación en paralelo la maneja `make -jN`; los `&` y `wait` son internos de `ejecuta.sh`.

---

## Referencia

Si utiliza este código en su investigación, por favor cite:

> García-Reynoso, J.A. et al. Inventario de Emisiones **Nacional** (año base 2008) para
> modelación de calidad del aire.
> Centro de Ciencias de la Atmósfera, UNAM.

---

## Autor

**José Agustín García Reynoso**
Centro de Ciencias de la Atmósfera, UNAM
📧 agustin@atmosfera.unam.mx
🔗 https://github.com/JoseAgustin
