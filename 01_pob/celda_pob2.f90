!
!	celda_pob.f90
!	
!
!	Created by Agustin on 28/05/12.
!	Copyright 2012 CCA-UNAM. All rights reserved.
!
!  Reads celda poblacion and group in a cel
!  ifort -o celda.exe celda_pob.f90
!
! modifications
!
!  Jan-10 2014  reads urban and rural total munc population
!
module vars
integer :: nl  ! line numbers in Pob_TOTAL_x_celda
integer,allocatable:: grid(:),grid2(:)
integer,allocatable::pr(:),pt(:),prT(:),puT(:)
integer,allocatable::pr2(:),pt2(:),prT2(:)
integer,allocatable ::pu(:),pu2(:),puT2(:)
character(len=5),allocatable::cemun(:),cemun2(:),cemun3(:)
common /var/ nl
end module
!
program celda
use vars

	call lee
	
	call calcula
	
	call guarda
contains
subroutine lee
	implicit none
	integer i,j,idum
	character(len=10)::cdum
    character (len=25):: fname
    fname='Pob_TOTAL_x_celda.csv'
	open (unit=10,file=fname,status='old',action='read')
    read(10,'(A)') cdum
	i=0
	do
	read(10,*,END=100) cdum
	i=i+1
	end do
100 continue
	rewind(10)
	nl=i
	print *,'file ',fname,' has',nl,'lines'
	allocate(grid(nl),pr(nl),pu(nl),pt(nl),cemun(nl))
    allocate(prT(nl),puT(nl))
	read(10,*)cdum
	do i=1,nl
	read(10,*)grid(i),cemun(i),pr(i),pu(i),pt(i),prT(i),puT(i)
    !print *,i,grid(i),prT(i),puT(i)
	end do
end subroutine lee
subroutine calcula
	implicit none
	integer ::i,j
	call count
    do j=1,nl
        do i=1,size(grid2)
			if(grid2(i).eq.grid(j).and.cemun2(i).eq.cemun(j))then
				pr2(i)=pr2(i)+pr(j)
				pu2(i)=pu2(i)+pu(j)
				pt2(i)=pt(j)
                prT2(i)= prT(j)
				puT2(i)= puT(j)
             !if(i.eq.100)print *,grid2(i), prT2(i),puT2(i),pr2(i),pu2(i),cemun2(i),pt2(i)
			end if
		end do
	end do
    
end subroutine calcula
!
subroutine count
!  Identifies the different elements in the array
  integer i,j,k
logical,allocatable::xl(:),xlm(:)
  allocate(xl(size(grid)),xlm(size(grid)))
  xl=.true.
  xlm=.true.
  do i=1,nl-1
   do j=i+1,nl
   if(grid(j).eq.grid(i).and.xl(j).and.cemun(j).eq.cemun(i)) xl(j)=.false.
   if(xlm(j).and.cemun(j).eq.cemun(i)) xlm(j)=.false.
   end do
  end do
  
  j=0
  do i=1,nl
    if(xl(i)) j=j+1
  end do
    allocate(grid2(j),pr2(j),pu2(j),pt2(j),cemun2(j))
    allocate(prT2(j ),puT2(j ))
  j=0
  do i=1,nl
    if(xl(i)) then
	j=j+1
	grid2(j)=grid(i)
	cemun2(j)=cemun(i)
	end if
  end do
  print *,'Number of different cells',j
  !print *,'Number of different muni',size(cemun3)
  deallocate(xl)
end subroutine count
subroutine guarda
	implicit none
	integer:: i,j
	real :: fu,fr
    real :: fu2,fr2
	open(unit=20,file='gri_pob.txt')
	write(20,'(A)')'GRIDCODE,ID,furb,frural,fpob, PobUrb, PobR, puT,prT, PobT'
	write(20,'(A)')'0			fraction'
    GRID: do i=1,size(grid2)
        if (pt2(i).eq.0) then
        fu=0
        fr=0
        fr2=0
        else
		fu= real(pu2(i))/real(pt2(i))
        fr= real(pr2(i))/real(pt2(i))
        fr2= real(pr2(i))/real(pt2(i))
        fu2= real(pu2(i))/real(pt2(i))
        end if
		write(20,300) grid2(i),cemun2(i),fu2,fr2,fu+fr,int(pu2(i)),pr2(i),puT2(i),prT2(i),pt2(i)
	end do GRID
300 format(i6,",",A5,2(",",F10.7),",",(F10.7,","),2(I6,","),2(I7,","),I7)
end subroutine guarda
end program celda
