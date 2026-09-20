!
!   crea_grib.f90
!   
! ifort -O2 -axAVX crea_grib.f90 -o crea_grib.exe
!
!   Created by Agustin Garcia on 30/01/17.
!   Copyright 2017 Centro de Ciencais de la Atmosfera, UNAM. All rights reserved.
!
! Proposito: Lee archivo de vegetacion y genera las coverturas de suelo
!
!  Modificaciones
!
module vars
integer ::nm
integer,allocatable :: grid(:),icve(:)
real,allocatable :: rural(:),urb(:),fpob(:)


common /vars1/ nm

end module vars

program pobalcion
use vars
integer:: i, np
real :: total
character(len=10)::cdum

    nm=300
    allocate(grid(nm),icve(nm),rural(nm),urb(nm),fpob(nm))
    open(unit=10,file='pob_rur_urb.csv',status='old',action='read')
    open(unit=20,file='grib_pob.csv')
      write(20,*)'GRIDCODE,ID,furb,frural,fpob, PobUrb, PobR, puT,prT, PobT'
!    Encabezado
     read (10,*) cdum
    do
        np=1+np
        read(10,*,end=133) cdum,grid(np),rural(np),urb(np),total
        !print *,cdum,grid(np),rural(np),urb(np),total

        if(np.gt.nm) stop 'np>nm'
        if (trim(cdum).eq.'Total') then
            do i=1,np-1
                fpob(i)=(rural(i)+urb(i))/(total+urb(np))
                if (total.eq.0 .and. urb(np).ne.0)  then
                 write(20,100) grid(i),grid(np),0.0,rural(i)/urb(np),fpob(i),urb(i),rural(i),total,urb(np),total+urb(np)
                else if (total.ne.0 .and.urb(np).eq.0 )  then
                 write(20,100) grid(i),grid(np),urb(i)/total,0.,fpob(i),urb(i),rural(i),total,urb(np),total+urb(np)
                else if (total.eq.0 .and.urb(np).eq.0 )  then
                 write(20,100) grid(i),grid(np),0.,0.,fpob(i),urb(i),rural(i),total,urb(np),total+urb(np)
                 else
                 write(20,100) grid(i),grid(np),urb(i)/total,rural(i)/urb(np),fpob(i),urb(i),rural(i),total,urb(np),total+urb(np)
                end if
            end do
            np=0
        end if
    end do
100 format(i7,",",i5,3(",",f9.7),5(",",f9.1))
133 continue
end program pobalcion
