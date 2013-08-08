!
!	s_biogenic.f90
!	
!
!	Created by Agustin on 10/07/12.
!	Copyright 2012 CCA-UNAM. All rights reserved.
!
!   Temporal distribution of biogenic emissions
!
module vars1
integer :: month,daytype
integer :: nf !number of emission files
integer :: nm ! line number in emissions file
integer :: nh ! number of hour per day
integer :: lh ! line number in uso horario
integer :: nnscc !max number of scc descriptors in input files
integer, allocatable :: idcel(:),idcel2(:)
integer, allocatable :: idsm(:),idsmh(:) ! state municipality IDs emiss and usoH
integer, allocatable :: mst(:)  ! Difference in number of hours (CST, PST, MST)
integer ::juliano
real, allocatable  ::isop(:),mono(:),otro(:),voc(:),NO(:),NOx(:)
parameter (nf=5,nh=24, nnscc=5,juliano=365)
integer,dimension(nf) :: nscc ! number of scc codes per file
integer*8,dimension(nnscc) ::iscc 
real,dimension(nnscc,nf) :: mes,dia
real,dimension(nnscc,nf,nh):: hCST,hMST,hPST
integer,dimension(3,nnscc,nf):: profile  ! 1=mon 2=weekday 3=hourly
character(len=3),dimension(juliano):: cdia
character (len=19) :: current_date

common /vv/nm,nscc,lh,month,daytype,mes,dia,hora,current_date
end module vars1
program spatial
use vars1

	call lee
	
	call calcula
	
	call guarda
	
contains

subroutine lee
	implicit none
	integer i,j,k,l,m
	integer idum,imon,iwk,ipdy,idia
	integer*8 jscc ! scc code from temporal file
	real  :: rdum
	integer,dimension(25) :: itfrc  !montly,weekely and hourly values and total
	integer,dimension(12) :: daym ! days in a month
	logical fil1
	character(len=10):: cdum
	character(len=18):: nfile
	! number of day in a month 
	!          jan feb mar apr may jun jul aug sep oct nov dec
	data daym /31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
 
	print *,"e_biogenicas.csv file"
	open (unit=10,file='e_biogenicas.csv',status='old',action='read')
	nm=0
	read(10,*)cdum  ! read heading
	read(10,*)cdum  ! read heading
	do
		read(10,*,end=100) cdum
		nm=nm+1
	end do
100 continue
	print *,'Archivo e_biogenicas.csv tiene',nm,'lineas'
	allocate(idcel(nm),isop(nm),mono(nm),otro(nm),voc(nm),NO(nm),NOx(nm))
	rewind(10)
	read(10,*)cdum  ! read heading
	read (10,*) nscc(k),cdum,(iscc(i),i=1,nscc(k))
	do i=1,nm
	 read(10,*) idcel(i),isop(i),mono(i),otro(i),voc(i),NO(i),NOx(i)
	end do
	print *,'   Termina lectura e_biogenicas.csv'
	close(10)
	print *,"READING fecha.txt file"
	open (unit=10,file='fecha.txt',status='OLD',action='read')
	read (10,*)month  
	read (10,*)idia
	month=abs(month)
	idia=abs(idia)
	if (month.lt.1 .or. month.gt.12) then
	print '(A,I3)','Error in month (from 1 to 12) month= ',month
	stop
	end if
	if (idia.gt.daym(month))then
	print '(A,I2,A,I2)','Error in day value: ',idia,' larger than days in month ',daym(month)
	Stop
	end if
	close(10)
    if(month.lt.10) then
        write(current_date,'(A6,I1,A12)')'2005-0',month,'-01_00:00:00'
        else
        write(current_date,'(A5,I2,A12)')'2005-',month,'-01_00:00:00'
    end if
    if(idia.lt.10) then
        write(current_date(10:10),'(I1)') idia
        else 
        write(current_date( 9:10),'(I2)') idia
    end if
    print *,'Done fecha.txt : ',current_date,month,idia
!
    print *,"READING uso_horario.csv file"
    open (unit=10,file='uso_horario.csv',status='OLD',action='read')
    lh=0
    read(10,*)cdum
        do
        read(10,*,end=90)cdum
        lh=lh+1
        end do
90  continue
    print *,'Line number in uso hor',lh
    allocate(idsmh(lh),mst(lh))
    rewind(10)
    read (10,'(A)') cdum
    do i=1,lh
    read (10,*) idsmh(i),mst(i)
    end do
    print *,'Done uso_horario.csv :'
    close(10)
!
!   Days in 2005 year
!
    print *,"READING anio2005.csv file"
    open (unit=10,file='anio2005.csv',status='OLD',action='read')
    daytype=0
    read(10,*)cdum
        do
        read(10,*,end=95)imon,ipdy,idum,cdum
		if(imon.eq.month.and. ipdy.eq.idia) then
		 daytype=idum
		 print *,'Day type :',daytype,cdum
		 exit
		 end if
        end do
95  continue
    close(10)
	if(daytype.eq.0) STOP 'Error in daytype=0'
!  REading and findig monthly, week and houry code profiles
    inquire(15,opened=fil1)
    if(.not.fil1) then
	  open(unit=15,file='temporal_01.txt',status='OLD',action='read')
	else
	  rewind(15)
	end if
	read (15,'(A)') cdum
      do
	  read(15,*,END=200)jscc,imon,iwk,ipdy
	    do i=1,nscc(k)
		  if(iscc(i).eq.jscc) then
		    profile(1,i,k)=imon
		    profile(2,i,k)=iwk
		    profile(3,i,k)=ipdy
		   end if
		end do
	  end do
 200 continue
      !print '(A3,<nscc(k)>(I5))','mon',(profile(1,i,k),i=1,nscc(k))      
      !print '(A3,<nscc(k)>(I3,x))','day',(profile(2,i,k),i=1,nscc(k))      
	  !print '(A3,<nscc(k)>(I3,x))','hr ',(profile(3,i,k),i=1,nscc(k))
	 print *,'   Done Temporal_01'
	 
!  REading and findig monthly  profile
    inquire(16,opened=fil1)
    if(.not.fil1) then
	  open(unit=16,file='temporal_mon.txt',status='OLD',action='read')
	else
	  rewind(16)
	end if
	read (16,'(A)') cdum
     do
	    read(16,*,END=210)jscc,(itfrc(l),l=1,13)
	    do i=1,nscc(k)
	      if(jscc.eq.profile(1,i,k)) then
	        mes(i,k)=real(itfrc(month))/real(itfrc(13))
	      end if
		end do !i
	 end do
	 mes=mes/daym(month)! days per month
 210 continue
	 !print '(A3,<nscc(k)>(f6.3))','mon',(mes(i,k),i=1,nscc(k))
	 print *,'   Done Temporal_mon'
!  REading and findig weekely  profile
    inquire(17,opened=fil1)
    if(.not.fil1) then
	  open(unit=17,file='temporal_week.txt',status='OLD',action='read')
	else
	  rewind(17)
	end if
	read (17,'(A)') cdum
     do
	    read(17,*,END=220)jscc,(itfrc(l),l=1,8)
	    do i=1,nscc(k)
	      if(jscc.eq.profile(2,i,k)) then
	        dia(i,k)=real(itfrc(daytype))/real(itfrc(8))
	      end if
		end do !i
	 end do
 220 continue
     !print '(A3,<nscc(k)>(f6.3))','day',(dia(i,k),i=1,nscc(k))
	 print *,'   Done Temporal_week'
	 if(daytype.gt.5) then
	 nfile='temporal_wkend.txt'
	 else
	 nfile='temporal_wkday.txt'
	 end if
!  REading and findig houlry  profile
    inquire(18,opened=fil1)
    if(.not.fil1) then
	  open(unit=18,file=nfile,status='OLD',action='read')
	else
	  rewind(18)
	end if
	read (18,'(A)') cdum
     do
	    read(18,*,END=230)jscc,(itfrc(l),l=1,25)
	    do i=1,nscc(k)
	      if(jscc.eq.profile(3,i,k)) then
		    m=6
		    do l=1,nh
	        hCST(i,k,m)=real(itfrc(l))/real(itfrc(25))
			m=m+1
			if(m.gt.nh)m=m-nh
            end do
            m=7
            do l=1,nh
            hMST(i,k,m)=real(itfrc(l))/real(itfrc(25))
            m=m+1
            if(m.gt.nh)m=m-nh
            end do
            m=8
            do l=1,nh
            hPST(i,k,m)=real(itfrc(l))/real(itfrc(25))
            m=m+1
            if(m.gt.nh)m=m-nh
			end do
	      end if
		end do !i
	 end do
 230 continue
     !do l=1,nh
      !print '(A3,x,I2,x,<nscc(k)>(f6.3))','hr',l,(hCST(i,k,l),i=1,nscc(k))
	 !end do
	 print *,'   Done ',nfile,daytype
	close(15)
	close(16)
	close(17)
	close(18)
	
 end subroutine lee
 subroutine calcula
 implicit none
 
 end subroutine calcula
 subroutine guarda
 implicit none
 
 end subroutine guarda
end program spatial