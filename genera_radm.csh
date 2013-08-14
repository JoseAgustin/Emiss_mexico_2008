#!/bin/csh
#
#  genera_fecha.csh
#
#
#  Creado por Jose Agustin Garcia Reynoso el 26/07/12.
#
#  Proposito:
#         Realiza la secuencia de pasos para generar diferentes fechas
#         del inventario de emisiones.
#  Modificaciones:
#         14/08/2013 Actualizacion para IE del 2008
#
set ProcessDir = /users/Datos/Documents/Proyectos/EMISIONES/mexico_2008
echo $ProcessDir
#
#  Build the fecha.txt file

@ mes = 3
@ dia = 10

while ( $dia <= 10 )
echo $dia
cd $PWD/04_temis

echo $PWD
#
if ( -e fecha.txt ) then
rm fecha.txt
endif
#ln -sf anio2008.csv anio2013.csv
#
cat << End_Of_File > fecha.txt
$mes       ! month jan =1 to dec=12
$dia       ! day in the month (from 1 to 28,30 or 31)
End_Of_File
#
echo ' '
echo '  Mes ='$mes 'DIA '$dia
#
cd ..
echo 'Movil Temporal distribution'
cd 06_temisM/
./Mtemporal.exe > movil.log &
echo 'Point Temporal distribution'
cd ../07_puntual/
./Puntual.exe >& puntual.log &
echo 'Area Temporal distribution'
cd ../04_temis/
./Atemporal.exe  >& area.log
cd ../05_semisM/
./MSpatial.exe >& movil.log
#
#echo 'Biogenic'
#cd ../12_biogenic
#./Btemporal.exe > biog.log&
#
echo 'Speciation distribution PM2.5'
#
cd ../09_pm25spec
./spm25p.exe > puntual.log &
./spm25m.exe >movil.log &
./spm25a.exe > area.log&
#
echo 'Speciation distribution VOCs'
#
cd ../08_spec
echo '   RADM2 *****'
ln -sf profile_radm2.csv profile_mech.csv
echo 'Movile'
./spm.exe > movil_radm2.log &
echo 'Puntual'
./spp.exe > puntual_radm2.log  &
echo 'Area '
./spa.exe > area_radm2.log 
#
echo ' Guarda'
cd ../10_storage
./radm2.exe  > radm2_bio.log &
#./saprc99bio.exe > saprc_bio.log
@ dia++
end 
