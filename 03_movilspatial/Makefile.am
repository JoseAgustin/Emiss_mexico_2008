include $(top_srcdir)/common.am

bin_PROGRAMS   = agrega fcft carr vial
agrega_SOURCES = agrega.f90
fcft_SOURCES   = crea_fcft.f90
carr_SOURCES   = suma_carretera.f90
vial_SOURCES   = suma_vialidades.f90

carr_FCFLAGS   = $(OPT2_FCFLAGS) $(FC_COMMON)
vial_FCFLAGS   = $(OPT2_FCFLAGS) $(FC_COMMON)
agrega_FCFLAGS = $(OPT2_FCFLAGS) $(FC_COMMON)
fcft_FCFLAGS   = $(OPT2_FCFLAGS) $(FC_COMMON)
