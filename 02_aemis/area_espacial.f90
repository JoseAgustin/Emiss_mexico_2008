!
!	area_espacial.f90
!	
!
!	Created by Agustin on 14/08/12.
!	Copyright 2012 CCA-UNAM. All rights reserved.
!
!  Reads lan use fracction per cell and land use tyepe and converts
!  to a one line.
!  ifort -o ASpatial.exe -O3 area_espacial.f90
!
!  4/03/2015  Correction in Terminasl 2801500002 and agricultural fires 2801500250
!
module land
    integer nl,nf,nm,nnscc,edo, mun
    parameter (nm=2454,nf=7,nnscc=57)
    integer,allocatable :: grib(:),idb(:)  ! Bosque
    integer,allocatable :: gria(:),ida(:)  ! Agricola
    integer,allocatable :: grip(:),idp(:)  ! Poblacion
    integer,dimension(nf) :: nscc
    integer,dimension (nf,nm):: iem
    real,allocatable ::fb(:),fa(:)! Fracciones Bosque Agricola
    real,allocatable ::fp1(:),fp2(:),fp3(:)!Fracc Urbana1, Rural2 y total3
!   Emisiones fuentes agricolas, bosques y poblacion grid, n, nnscc
    real,allocatable :: eagr(:,:,:), ebos(:,:,:), epob(:,:,:)
    real,dimension(nm,nnscc,nf):: emiss
    character(len=10),dimension(nf,nnscc) ::scc
    character(len=25), allocatable :: desc(:)
    character(len=14),dimension(nf) ::efile,ofile
!   Emissions Inventory files
    data efile /'INH3_2008.csv','INOx_2008.csv','ISO2_2008.csv',&
&           'IVOC_2008.csv','ICO__2008.csv','IPM10_2008.csv',&
&           'IPM25_2008.csv'/
!            NH3          NO2         SO2    VOC    CO PM10 PM25
    data ofile /'ANH3_2008.csv','ANOx_2008.csv','ASO2_2008.csv',&
&           'AVOC_2008.csv','ACO__2008.csv','APM10_2008.csv',&
&           'APM25_2008.csv'/
end module land

program area_espacial
use land
       call lee

       call calculos

       call guarda

contains

subroutine lee
implicit none
    integer i,j,k
    character(len=12):: cdum,fname
    fname='bosque.csv'
    print *,'Lee ',fname
    open(unit=10,file=fname,status='OLD',action='read')
    read (10,*) cdum
    nl=0
    do
        read(10,*,end=100) cdum
        nl=nl+1
    end do
100 print *,'numero de lineas',nl
    allocate(grib(nl),idb(nl),fb(nl))
    rewind(10)
    read (10,*) cdum
    do i=1,nl
        read(10,*)grib(i),idb(i),fb(i)
    end do
    close(10)
!
    fname='agricola.csv'
    print *,'Lee ',fname
    open(unit=10,file=fname,status='OLD',action='read')
    read (10,*) cdum
    nl=0
    do
    read(10,*,end=110) cdum
    nl=nl+1
    end do
110 print *,'numero de lineas',nl
    allocate(gria(nl),ida(nl),fa(nl))
    rewind(10)
    read (10,*) cdum
    do i=1,nl
    read(10,*)gria(i),ida(i),fa(i)
    end do
    close(10)
!
    fname='gri_pob.csv'
    print *,'Lee ',fname
    open(unit=10,file=fname,status='OLD',action='read')
    read (10,*) cdum
    read (10,*) cdum
    nl=0
    do
        read(10,*,end=120) cdum
        nl=nl+1
    end do
120 print *,'numero de lineas',nl
!    Population fraction fp1 furb, fp2 frural, fp3 fpob
    allocate(grip(nl),idp(nl),fp1(nl),fp2(nl),fp3(nl))
    rewind(10)
    read (10,*) cdum
    read (10,*) cdum
    do i=1,nl
        read(10,*)grip(i),idp(i),fp1(i),fp2(i),fp3(i)
        ! GRIDCODE ID urb,frural,fpob
    end do
    close(10)
!
    do k=1,nf
        open (unit=10,file=efile(k),status='OLD',action='read')
        read (10,'(A)') cdum
        read (10,'(A)') cdum
        print *,efile(k)
        read (10,*) nscc(k),cdum,(scc(k,i),i=1,nscc(k))
        print '(5(A10,x))',(scc(k,i),i=1,nscc(k))
        print *,k,nscc(k)
        do i=1,nm
          read(10,*) edo,mun,iem(k,i),(emiss(i,j,k),j=1,nscc(k))
       end do ! i
        close(10)
    end do! k
end subroutine lee
subroutine calculos
    implicit none
    integer i,j,k,l,m
	allocate(eagr(size(gria),nf,nnscc))
	allocate(ebos(size(grib),nf,nnscc))
    allocate(epob(size(grip),nf,nnscc))
    eagr=0.0
    ebos=0.0
    epob=0.0
    print *," Inicia Calculos"
    open(unit=123,file="mass_balance.txt",status='UNKNOWN',action='write')
    write(unit=123,FMT=*) "   Balance de Materia"
    Clase: do k=1,nf
    print *,"     Agricola  ", efile(k)
    agricola: do j=1,size(fa) ! grid
    inven: do i=1,nm          ! municipality
        if(ida(j).eq.iem(k,i)) then
           do l=1,nscc(k)     ! SCC
             if(scc(k,l).eq.'2801500250') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! conversion de Mg to g.
             if(scc(k,l).eq.'2801000002') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! conversion de Mg to g.
             if(scc(k,l).eq.'2801000000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! conversion de Mg to g.
             if(scc(k,l).eq.'2801700000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! conversion de Mg to g.
             if(scc(k,l).eq.'2805000000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! conversion de Mg to g.
             if(scc(k,l).eq.'2805020000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! conversion de Mg to g.
             if(scc(k,l).eq.'2270005000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! conversion de Mg to g.
             if(scc(k,l).eq.'2461850000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6
             if(scc(k,l).eq.'2267000000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6 ! comb agricola GLP.
           end do
           exit inven
        end if
    end do inven
    end do agricola
    print *,"     Bosque"
    Bosque: do j=1,size(fb) ! grid
    invenb: do i=1,nm       ! municipality
        if(idb(j).eq.iem(k,i)) then
           do l=1,nscc(k)      ! SCC
             if(scc(k,l).eq.'2810001000') ebos(j,k,l)=emiss(i,l,k)*fb(j)*1e6 ! conversion de Mg to g.
           end do
           exit invenb
        end if
    end do invenb
    end do Bosque
        print *,"     Poblacion"
    poblacion: do j=1,size(grip)! grid
    invenp: do i=1,nm       ! municipality
        if(idp(j).eq.iem(k,i)) then
            do l=1,nscc(k)
            if(scc(k,l).eq.'2104007000') epob(j,k,l)=emiss(i,l,k)*(fp2(j)*0.2+fp1(j)*0.8)*1e6!Comb_res_LPG
            if(scc(k,l).eq.'2103006000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !Comb_comer_NG
            if(scc(k,l).eq.'2103007000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !Comb_comer_LPG
            if(scc(k,l).eq.'2104006000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !Comb Domet. NG
            if(scc(k,l).eq.'2415000000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !limpieza
            if(scc(k,l).eq.'2420000055') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !LAVADO EN SECO
            if(scc(k,l).eq.'2425000000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !Imprenta
            if(scc(k,l).eq.'2425000000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !Serigrafia
            if(scc(k,l).eq.'2425010000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !Offset
            if(scc(k,l).eq.'2425010000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !Litografia
            if(scc(k,l).eq.'2425030000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !Rotograbado
            if(scc(k,l).eq.'2425040000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !FlexografÕa
            if(scc(k,l).eq.'2465400000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !Productos de cuidado automotriz
            if(scc(k,l).eq.'2501000000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !LPG
            if(scc(k,l).eq.'2801500002') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6 !Terminales de autobuses
            if(scc(k,l).eq.'2104008000') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1e6 !Comb_res_lena
            if(scc(k,l).eq.'2104011000') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1e6 !Comb_res_keroseno
            if(scc(k,l).eq.'2102004000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Comb_ind_Diesel
            if(scc(k,l).eq.'2102007000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Comb_ind_LPG
            if(scc(k,l).eq.'2201070000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Terminal Buses
            if(scc(k,l).eq.'2222222222') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Cruces_front
            if(scc(k,l).eq.'2260002000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Maq Contruc
            if(scc(k,l).eq.'2275000000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Aviacion
            if(scc(k,l).eq.'2275050000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Equipo basico aeropouertos
            if(scc(k,l).eq.'2280000000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Embarcaciones marinas
            if(scc(k,l).eq.'2285000000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Locomotoras de arrastre
            if(scc(k,l).eq.'2285002010') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Locomotoras de patio
            if(scc(k,l).eq.'2302002000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Asados al carbon
            if(scc(k,l).eq.'2302050000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Panificacion
            if(scc(k,l).eq.'2311010000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Construccion
            if(scc(k,l).eq.'2401001000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Recub Arq
            if(scc(k,l).eq.'2401005000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Pintado automotriz
            if(scc(k,l).eq.'2401008000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Senializacion
            if(scc(k,l).eq.'2401020000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !INDUSTRIA DE LA MADERA
            if(scc(k,l).eq.'2401050000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !FABRICACION DE PRODUCTOS METALICOS
            if(scc(k,l).eq.'2401055000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !FABRICACION DE MAQUINARIA Y EQUIPO
            if(scc(k,l).eq.'2401065000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !335 FABRICACION
            if(scc(k,l).eq.'2401080000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Fab eq transport
            if(scc(k,l).eq.'2401100000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Ind met basc
            if(scc(k,l).eq.'2401990000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Otr Ind Manuf
            if(scc(k,l).eq.'2401990000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !FABRICACION DE MUEBLES
            if(scc(k,l).eq.'2461020000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Asfaltado
            if(scc(k,l).eq.'2465000000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Productos en aerosol
            if(scc(k,l).eq.'2465100000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Productos_personal
            if(scc(k,l).eq.'2465200000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Productos dom_sticos
            if(scc(k,l).eq.'2465600000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Adhesivos y selladores
            if(scc(k,l).eq.'2465800000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Pesticidas comerciales y dom_sticos
            if(scc(k,l).eq.'2465900000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Productos miscelaneos
            if(scc(k,l).eq.'2501060000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Gasol
            if(scc(k,l).eq.'2630030000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !PTArs
            if(scc(k,l).eq.'2810030000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Incendios
            if(scc(k,l).eq.'2850000010') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Hospi
            if(scc(k,l).eq.'5555555555') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6 !Uso_domestico
           end do
            exit invenp
        end if
    end do invenp
    end do poblacion
! Balance de materia
write(unit=123,FMT=*) ofile(k),sum(eagr(:,k,:))+sum(epob(:,k,:))+&
&        sum(ebos(:,k,:)),&
&        sum(emiss(:,:,k))*1e6

    end do Clase
end subroutine calculos
subroutine guarda
    implicit none
    integer i,k,l
    Print *,"   ***   Guarda   ***"
    do k=1,nf
        open(unit=10,file=ofile(k),ACTION='write')
        write(10,*)'grid,CID,Furb,Frural,SCCs'
        write(10,300)nscc(k),(scc(k,i),i=1,nscc(k))
        print *,"   Agricola ",ofile(k)
        do i=1,size(fa)
            write(10,310) gria(i),ida(i),0.,fa(i),(eagr(i,k,l),l=1,nscc(k))
        end do
        print *,"   Bosque"
        do i=1,size(fb)
            write(10,310) grib(i),idb(i),0.,fb(i),(ebos(i,k,l),l=1,nscc(k))
        end do
        print *,"   Poblacion"
        do i=1,size(fp1)
            write(10,310) grip(i),idp(i),fp1(i),fp2(i),(epob(i,k,l),l=1,nscc(k))
        end do

       close(10)
    end do
#ifndef PGI
300 format(I3,", g_per_year",<nnscc>(",",A10))
310 format(I9,",",I6,",",F,",",F,<nnscc>(",",ES12.5))
#else
300 format(I3,", g_per_year",60(",",A10))
310 format(I9,",",I6,",",F7.4,",",F7.4,57(",",ES12.5))
#endif

end subroutine guarda
end program area_espacial

