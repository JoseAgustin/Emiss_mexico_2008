!
!   genera_covertura.f90
!   
! ifort -O2 -axAVX genera_covertura.f90 -o covertura.exe
!
!   Created by Agustin Garcia on 27/01/17.
!   Copyright 2017 Centro de Ciencais de la Atmosfera, UNAM. All rights reserved.
!
! Proposito: Lee archivo de vegetacion y genera las coverturas de suelo
!
!  Modificaciones
!
module vars
integer ::nm
integer,allocatable :: grid(:),icve(:)
real,allocatable :: agr(:),veg(:),urb(:)


common /vars1/ nm

end module vars

program coverturas
use vars
integer:: i,np
real :: total
character(len=10)::cdum

    nm=793
    allocate(grid(nm),icve(nm),agr(nm),veg(nm),urb(nm))
    open(unit=10,file='MEXICO2008_uso_suelo.csv',status='old',action='read')
    open(unit=20,file='agricola2.csv')
    open(unit=30,file='vegetaci.csv')
    open(unit=40,file='urbano.csv')
    write(20,*)'GRIDCODE,CVIDE,Fa,USs4ginMUNICIPIO (m2)'
    write(30,*)'GRIDCODE,CVIDE,Fv,USs4ginMUNICIPIO (m2)'
    write(40,*)'GRIDCODE,CVIDE,Fu,USs4ginMUNICIPIO (m2)'
    ! Encabezado
    read (10,*) cdum
    read (10,*) cdum
    np=0
    do
     np=1+np
     read(10,*,end=133) cdum,grid(np),agr(np),veg(np),urb(np),total
     if(np.gt.nm) stop 'np>nm'
     if (trim(cdum).eq.'Total') then
        do i=1,np-1
         if (veg(np).ne.0 .or.agr(i).ne.0)  then
            write(20,*) grid(i),",",grid(np),",",agr(i)/veg(np),",",veg(np)
         end if
         if (urb(np).ne.0 .or.veg(i).ne.0)then
            write(30,*) grid(i),",",grid(np),",",veg(i)/urb(np),",",urb(np)
         end if
         if(total.ne.0 .or. urb(i).ne.0 ) then
            write(40,*) grid(i),",",grid(np),",",urb(i)/total,",",total
         end if
        end do
        np=0
     end if
    end do
133 continue
end program
