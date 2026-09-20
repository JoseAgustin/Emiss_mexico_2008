include $(top_srcdir)/common.am

# check.F90 usa la interfaz Fortran de NetCDF
AM_FCFLAGS += $(NETCDF_FCFLAGS)
LDADD       = $(NETCDF_LIBS)

bin_PROGRAMS  = Aanual Mannual Panual

Aanual_SOURCES  = check.F90 master_mod.F90 areaanual.F90
Mannual_SOURCES = check.F90 master_mod.F90 movil_anual.F90
Panual_SOURCES  = check.F90 master_mod.F90 f_anual.F90
