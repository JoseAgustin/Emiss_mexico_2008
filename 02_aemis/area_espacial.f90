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
          if(edo.eq.17 .or.edo .eq.13 .or.edo.eq.29)then!morelos hidalgo tlax
              do j=1,nscc(k)
              emiss(i,j,k)= 3* emiss(i,j,k)
              end do ! j
          end if
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
    Clase: do k=1,nf
    print *,"     Agricola  ", efile(k)
    agricola: do j=1,size(fa) ! grid
    inven: do i=1,nm          ! municipality
        if(ida(j).eq.iem(k,i)) then
           do l=1,nscc(k)     ! SCC
             if(scc(k,l).eq.'2801500002'.or.scc(k,l).eq.'2801000002'.or.&
                scc(k,l).eq.'2801000000'.or.scc(k,l).eq.'2267000000'.or.&
                scc(k,l).eq.'2801700000'.or.scc(k,l).eq.'2805000000'.or.&
                scc(k,l).eq.'2270005000') then
                eagr(j,k,l)=emiss(i,l,k)*fa(j)*1000  ! conversion de Mg to Kg
             end if
             if(scc(k,l).eq.'2199007000')eagr(j,k,l)=emiss(i,l,k)*fa(j)*1000*0.8
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
             if(scc(k,l).eq.'2810001000') ebos(j,k,l)=emiss(i,l,k)*fb(j)*1000
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
            epob(j,k,l)=emiss(i,l,k)*fp1(j)*1000
            if(scc(k,l).eq.'2104008000') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1000
            if(scc(k,l).eq.'2104011000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1000
            if(scc(k,l).eq.'2199007000') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1000*0.2
            if(scc(k,l).eq.'2222222222') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1000
            if(scc(k,l).eq.'2302002000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1000
            if(scc(k,l).eq.'2461800000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1000
            if(scc(k,l).eq.'2801700000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1000
            if(scc(k,l).eq.'2801000001') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1000
            if(scc(k,l).eq.'2805000000') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1000
            if(scc(k,l).eq.'2805001100') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1000
            if(scc(k,l).eq.'2805020000') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1000
            if(scc(k,l).eq.'2810001000') epob(j,k,l)=0.0 !bosque
            if(scc(k,l).eq.'2801500002') epob(j,k,l)=0.0 !Agricola
            end do
            exit invenp
        end if
    end do invenp
    end do poblacion
    end do Clase
end subroutine calculos
subroutine guarda
    implicit none
    integer i,k,l
    Print *,"Guarda"
    do k=1,nf
        open(unit=10,file=ofile(k),ACTION='write')
        write(10,*)'grid,CID,Furb,Frural,SCCs'
        write(10,300)nscc(k),(scc(k,i),i=1,nscc(k))
        print *,"   Agricola ",ofile(k)
        do i=1,size(fa)
            write(10,310) gria(i),ida(i),0,fa(i),(eagr(i,k,l),l=1,nscc(k))
        end do
        print *,"   Bosque"
        do i=1,size(fb)
            write(10,310) grib(i),idb(i),0,fb(i),(ebos(i,k,l),l=1,nscc(k))
        end do
        print *,"   Poblacion"
        do i=1,size(fp1)
            write(10,310) grip(i),idp(i),fp1(i),fp2(i),(epob(i,k,l),l=1,nscc(k))
        end do

       close(10)
    end do
300 format(I3,", kg_per_year",<nnscc>(",",A10))
310 format(I9,",",I6,",",F,",",F,<nnscc>(",",ES12.5))
end subroutine guarda
end program area_espacial

