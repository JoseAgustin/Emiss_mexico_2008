!
!	sumamovil.f90
!	
!
!	Created by Agustin Garcia on 13/02/12.
!	Copyright 2012 CCA-UNAM. All rights reserved.
!

!  programa que lee fuentes moviles y las suma

	program sumamovil
	integer nd,ne
	parameter(nd=298040,ne=7)
	character (len=10),dimension(nd) :: scc	
	character (len= 7),dimension(nd) :: fips
	character (len= 4),dimension(ne) :: cemis
	character (len=10) ::sccb
	character (len= 5) ::fipsb
	character(len=1) :: st
	character(len=13) :: ofile
	integer:: anio,est,cint
	integer :: i,j
	real,dimension(ne):: emisb
	real,dimension(nd,ne):: emis
	data ofile /'M_orl2008.txt'/

	data cemis/'PM10','PM25','NOx','SO2','CO','VOC','NH3'/

	call lee
	
	call guarda
	
	contains
	subroutine lee
	integer ::i,j
	character (len=10):: cdum
	print *,"     Lee datos"
	open (unit=10,file='F_moviles.csv',status='OLD',action='read')
	read (10,*)cdum
	do i=1,nd
	read (10,*)anio,est,st,fips(i),cdum,cdum,scc(i),cint,cint,(emis(i,j),j=1,ne)
	print *,i,anio," ",est," ",st," ",fips(i)," ",scc(i)!," ",(emis(j),j=1,ne)
	end do
    close(10)
	end subroutine
!
	subroutine guarda
    print *,"     Hace SUMA"
		open (unit=20,file=ofile)
		write(20,100)

	 fipsb=fips(1)
	 sccb =scc(1)
    do i=1,ne
	 emisb(i)=emis(1,i)
	end do
!
	do i=2,nd
	  if( fipsb.eq.fips(i) .and.sccb.eq.scc(i)) then
        do j=1,ne
			emisb(j)=emis(i,j)+emisb(j)
	    end do
	  else
			emisb=emisb/0.90718474  !conversion a ton short
		    do j=1,ne
			 if(emisb(j).ne.0) then
			 select case (j)
			 case( 1, 2)
			 write(20,120) fipsb,trim(sccb),cemis(j),emisb(j),emisb(j)/250,'04'
			 case( 3,4,6,7)
			 write(20,121) fipsb,trim(sccb),cemis(j),emisb(j),emisb(j)/250,'04'
			 case(5)
 			 write(20,122) fipsb,trim(sccb),cemis(j),emisb(j),emisb(j)/250,'04'
			 end select
			 end if
			end do
			do j=1,ne
				emisb(j)=emis(i,j)
				fipsb=fips(i)
				sccb =scc(i)
			end do
	  end if
	end do
	close(20)
100 format('#ORL  '/'#TYPE     Mobile Source Criteria Inventory'/&
&'#COUNTRY  MX'/'#YEAR     2008'/'#DESC     Draft 2008 version 1.0')
120 format(A5,",",A10,",",A4,",",F,",",F,",,,,",A2,',semarnat_computed,2008')
121 format(A5,",",A10,",",A3,",",F,",",F,",,,,",A2,',semarnat_computed,2008')
122 format(A5,",",A10,",",A2,",",F,",",F,",,,,",A2,',semarnat_computed,2008')	 
	end subroutine
	end program