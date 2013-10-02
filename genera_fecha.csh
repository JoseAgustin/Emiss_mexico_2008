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
#
set ProcessDir = $PWD
#
#  Build the fecha.txt file

@ mes = 4
@ dia = 8	
while ($dia <= 11 )        
cd $ProcessDir

#
if ( -e 04_temis/fecha.txt ) then
rm 04_temis/fecha.txt
endif
#
cat << End_Of_File > 04_temis/fecha.txt
$mes       ! month jan =1 to dec=12
$dia       ! day in the month (from 1 to 28,30 or 31)
End_Of_File
#
echo ' '
echo '  Mes ='$mes 'DIA '$dia
#
echo 'Movil Temporal distribution'
cd 06_temisM/
./Mtemporal.exe > movil.log &
echo 'Point Temporal distribution'
cd ../07_puntual/
./Puntual.exe> puntual.log &
echo 'Area Temporal distribution'
cd ../04_temis/
./Atemporal.exe > area.log 
#cd ../05_semisM/
#./MSpatial.exe

echo 'Speciation distribution VOCs'
#
cd ../08_spec
echo '   RADM2 *****'
ln -sf profile_radm2.csv profile_mech.csv
echo 'Puntual'
./spp.exe > puntual_radm2.log &
echo 'Movile'
./spm.exe > movil_radm2.log &
echo 'Area '	
./spa.exe > area_radm2.log 
echo '***** SAPRC99'
ln -sf profile_saprc99.csv profile_mech.csv
echo 'Puntual'
./spp.exe > puntual_saprc99.log  &
echo 'Movile'
./spm.exe > movil_saprc99.log & 
echo 'Area '
./spa.exe > area_saprc99.log 
#
echo 'Speciation distribution PM2.5'
#
cd ../09_pm25spec
./spm25p.exe > puntual.log &
./spm25m.exe >movil.log &
./spm25a.exe > area.log
echo 'Biogenic'
cd ../12_biogenic
./Btemporal.exe > biog.log
echo ' Guarda'
cd ../10_storage
./radm2bio.exe  > radm2_bio.log &
./saprc99bio.exe > saprcbio.log
@ dia++
end 
