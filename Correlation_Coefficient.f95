program corr_coeff

  ! It calculates the correlation coefficient between the columns 1-2, 1-3, 1-4 in fileB.prn.

  integer i, j
  real B(43,4),R(3)

  !read from file B
  open(1, file='fileB.prn')
  do i=1,43
     read(1,*)(B(i,j),j=1,4)
  end do
  close(1)
  
  call correlation(B, 43, 4, R)

  !write the correlation coefficient for every column
  write(6,*)'The correlation coefficient for every column is:'
  do j=1,3
     write(6,*)'For columns 1,', j+1, 'is:', R(j)
  end do
  

end program corr_coeff

!--------------------------------
subroutine correlation(v, r, c, coef)

  !calculates the correlation coefficient between two variables

  integer i, j, r, c
  real v(r,c), coef(c-1), m(c), dif(r,c), sum, num, den1, den2, den

  !calculate mean of values for each column
  do j=1,c
     sum=0
     do i=1,r
        sum=sum+v(i,j)
     end do
     m(j)=sum/r
  end do

  !calculate difference between xi and mean(x)
  do j=1,c
     do i=1,r
        dif(i,j)=v(i,j)-m(j)
     end do
  end do
 
  !calculation of correlation coefficient
  do j=2,c
     num=0
     den1=0;den2=0
     do i=1,r
        num=num+dif(i,1)*dif(i,j)
        den1=dif(i,1)**2+den1
        den2=dif(i,j)**2+den2
     end do
     den=sqrt(den1*den2)
     coef(j-1)=num/den
     !write(6,*)num,den
  end do
  
  return

end subroutine correlation
