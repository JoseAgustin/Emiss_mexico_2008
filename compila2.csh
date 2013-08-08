cd 10_storage
set FC=`nc-config --fc`
set FFL=`nc-config --fflags`
set FLL=`nc-config --flibs`
echo $FC' '$FFL' '$FLL' '-O2 -axAVX -fp-model precise guarda2bio_nc4.f90 -o radm2bio4.exe
`ifort' '$FFL' '$FLL' '-O2 -axAVX -fp-model precise guarda2bio_nc4.f90 -o radm2bio4.exe`
cd ..
cd 12_biogenic
ifort -O3 -axAVX -fp-model precise  btemporal.f90 -o Btemporal.exe
