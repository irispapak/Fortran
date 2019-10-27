program wind

  implicit none
  
  integer i,j
  real A(20,2), ws(20), wdir(20), rad

  open(1, file='TableA.txt')
  do i=1,20
     read(1,*)(A(i,j),j=1,2)
  end do
  close(1)

  rad=4.0*atan(1.0)/180.0
  do i=1,20
     ws(i)=sqrt(A(i,1)**2 + A(i,2)**2)
     wdir(i)=atan2(A(i,2),A(i,1))/rad+180     
  end do

  open(4, file='wind.txt')
  write(4,2)' u','v','norm','direction'
  do i=1,20
     write(4,3)(A(i,j),j=1,2), ws(i), wdir(i)
  end do
  close(4)
  
  2 format (a,8x,a,6x,a,3x,a)
  3 format (f6.3,2x,f6.3,2x,f6.3,2x,f7.3)

  
end program wind
