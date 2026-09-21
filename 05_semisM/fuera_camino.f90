!
!	fuera_camino.f90
!	
!
!	Created by Agustin Garcia on 14/02/12.
!	Copyright 2012 CCA-UNAM. All rights reserved.
!

!!

program fuera_camino
implicit none
integer ::np,ne
parameter (np=17177)  ! numero de lineas en el archivo
parameter (ne=7)      ! numero de especies en el inventario
integer,dimension(np) :: edo,mun  !identificador de estado y municipio
character(len=10),dimension(np) ::scc   ! clave scc
character(len=4),dimension(ne) :: name  ! nombre de la especie en el inventario
real, dimension(np,ne) :: emiss

	call lee
	
	call guarda
contains
	subroutine lee
		character(len=10)::cdum
		integer i,j
		open (unit=10, file='Nonroad.csv',status='OLD',action='read')
		do i=1,3
		read(10,'(A)') cdum   ! lee encabezado
		end do
		read (10,*) cdum,cdum,cdum,cdum,cdum,cdum,(name(j),j=1,ne)
		print *,name
		do i=1,np
		read (10,*) edo(i),cdum,mun(i),cdum,cdum,scc(i),(emiss(i,j),j=1,ne)
		end do
		close(10)
		emiss=emiss/0.90718474
	end subroutine
	subroutine	guarda
		integer ::i,j
		character (len=5)::fips
		open(unit=20,file='MnR_orl2005.txt',action='write')
		write(20,100) 
		do i=1,np
			call dofips(edo(i),mun(i),fips)
			do j=1,ne
			 if(emiss(i,j).ne.0) then
			 select case (j)
			 case( 1, 2) ! PM10 PM25
			 write(20,120) fips,scc(i),name(j),emiss(i,j),emiss(i,j)/250,'03'
			 case( 3,4,6,7) !NOx, SO2,COV,NH3
			 write(20,121) fips,scc(i),name(j),emiss(i,j),emiss(i,j)/250,'03'
			 case(5)       !  CO
 			 write(20,122) fips,scc(i),name(j),emiss(i,j),emiss(i,j)/250,'03'
			 end select
			 end if

			end do
		end do
		close(20)
	100 format('#ORL  '/'#TYPE    Nonroad Source Criteria Inventory'/&
&'#COUNTRY  MX'/'#YEAR     2005'/'#DESC     Draft 2005 version 1.0')	
120 format(A5,",",A10,",",A4,",",F0.4,",",F0.4,",,,,",A2,',semarnat_computed,2005,')	 
121 format(A5,",",A10,",",A3,",",F0.4,",",F0.4,",,,,",A2,',semarnat_computed,2005,')	 
122 format(A5,",",A10,",",A2,",",F0.4,",",F0.4,",,,,",A2,',semarnat_computed,2005,')	 
 
	end subroutine
	!
    subroutine dofips(ii,jj,cc)
	integer,intent(IN)::ii,jj
	character(len=5),intent(OUT)::cc
	if (ii.lt.10)then
		write(cc(1:1),'(I1)')0
		write(cc(2:2),'(I1)') ii
	else
		write(cc(1:2),'(I2)') ii
	end if
	if(jj.lt.100) then
		if(jj.lt.10) then
			write(cc(3:4),'(A2)')"00"
			write(cc(5:5),'(I1)') jj
		else
			write(cc(3:3),'(I1)') 0
			write(cc(4:5),'(I2)') jj
		end if
	else
		write(cc(3:5),'(I3)') jj
	endif
	end subroutine
!
!
end program
