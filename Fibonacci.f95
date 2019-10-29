program fibonacci

  ! It creates a Fibonacci sequence based on the length of numbers given by the user.

  integer N, i
  integer, dimension(:), allocatable :: a

  write(6,*)'Give the length of Fibonacci sequence:'
  read(5,*) N

  allocate(a(N))
  a(1)=0
  a(2)=1
  
  do i=1,N-2
     a(i+2)=a(i)+a(i+1)
  end do

  write(6,*)'The Fibonacci sequence is:', a

end program fibonacci
