program K_index
  
  implicit none

  real A(36,11), K ,t850, td850, t700, td700, t500  !,T(36),Td(36)
  integer i,j

  open(1, file='Data_radiosonde_5_5_2017.txt')
  do i=1,36
     read(1,*)(A(i,j),j=1,11)
  end do
  close(1)

  call Kindex(A,36,11,K,t850,td850,t700,td700,t500)

  write(6,*)'T850 =',t850,'C'
  write(6,*)'Td850 =',td850,'C'
  write(6,*)'T700 =',t700,'C'
  write(6,*)'Td700 =',td700,'C'
  write(6,*)'T500 =',t500,'C'
  write(6,*)'K index =',K

  if (K<20) then
     write(6,*)'No possibility of rain'
  else if (K<=25) then
     write(6,*)'Isolated thunderstorms'
  else if (K<=30) then
     write(6,*)'Widely scattered thunderstorms'
  else if (K<=35) then
     write(6,*)'Scattered thunderstorms'
  else
     write(6,*)'Numerous thunderstorms'
  end if
    


end program K_index

!***********************************************************
subroutine Kindex(B, n, m, K, t850, td850, t700, td700, t500)

  integer i, n, m 
  real B(n,m), K, t850, td850, t700, td700, t500

  do i=1,n
     if (B(i,1)==850) then
        t850=B(i,3)
        td850=B(i,4)
     else if (B(i,1)==700) then
        t700=B(i,3)
        td700=B(i,4)
     else if (B(i,1)==500) then
        t500=B(i,3)
     end if
  end do
  
  K=(t850-t500)+td850-(t700-td700)

  return


end subroutine Kindex
