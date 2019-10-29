program central_moving_average

  character (len=90) :: filename
  integer i, j, z
  real B(43,4),cma(43,4,4)

  !read from file B
  open(1, file='fileB.prn')
  do i=1,43
     read(1,*)(B(i,j),j=1,4)
  end do
  close(1)

  call cen_mov_average(B, 43, 4, cma)

  do z=1,4
     write(filename,*) 'cma',z,'.txt'
     open(2, file=filename)
     do i=1,43
        write(2,9)(cma(i,j,z),j=1,4)
     end do
     close(2)
  end do
  9 format(4f10.2)


end program central_moving_average

!----------------
subroutine cen_mov_average(A, r, c, cma)

  !calculates the central moving average for 3,5,7,9
  integer i, j, r, c, jump, z
  real A(r,c), cma3(r,c), cma5(r,c), cma7(r,c), cma9(r,c), cma(r,c,4)

  forall(i=1:r, j=1:c, z=1:4) cma(i,j,z)=0

  !----------------------------------------- 1st way
  !moving average 3
  do j=1,c
     do i=1,r-2
        cma3(i+1,j)=sum(A(i:i+2,j))/3
        !cma(i+1)=(A(i,1)+A(i+1,1)+A(i+2,1))/3
     end do
  end do
  
  !moving average 5
  do j=1,c
     do i=1,r-4
        cma5(i+2,j)=sum(A(i:i+4,j))/5
     end do
  end do
  
  !moving average 7
  do j=1,c
     do i=1,r-6
        cma7(i+3,j)=sum(A(i:i+6,j))/7
     end do
  end do

  !moving average 9
  do j=1,c
     do i=1,r-8
        cma9(i+4,j)=sum(A(i:i+8,j))/9
     end do
  end do

  !------------------------------------------ 2nd way
  jump=3
  do z=1,4
     do j=1,c
        do i=1,r+1-jump
           cma(i+z,j,z)=sum(A(i:i+jump-1,j))/jump 
        end do
     end do
     jump=jump+2
  end do
  
 
  return

end subroutine cen_mov_average
