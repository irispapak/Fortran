program percentiles

  ! It finds the 90th percentile of maximum temperature and 10th percentile of minimum temperature from file Thessaloniki.txt.
  
  implicit none
  
  integer i, j, n, max10, min10
  real A(15706,6), d9, d1, Tmax(15706), Tmin(15706), temp

  n=15706
  open(1, file='Thessaloniki.txt')
  do i=1,n
     read(1,*)(A(i,j),j=1,6)
  end do
  close(1)
  
  Tmin=A(:,4)
  Tmax=A(:,5)
 
  !-----sort Tmax
  do j=1,n-1
     do i=1,n-1
        if (Tmax(i+1)>Tmax(i)) then
           temp=Tmax(i)
           Tmax(i)=Tmax(i+1)
           Tmax(i+1)=temp
        end if
     end do
  end do

  max10=ifix(0.1*n)
  do i=1,max10
     d9=Tmax(i)
  end do

  write(6,*)'The 90th percentile of maximum temperature is:', d9, 'C'

  !-----sort Tmin
  do j=1,n-1
     do i=1,n-1
        if (Tmin(i+1)<Tmin(i)) then
           temp=Tmin(i)
           Tmin(i)=Tmin(i+1)
           Tmin(i+1)=temp
        end if
     end do
  end do
  
  min10=ifix(0.1*n)
  do i=1,min10
     d1=Tmin(i)
  end do

  write(6,*)'The 10th percentile of minimum temperature is:', d1, 'C'
  
  
end program percentiles
