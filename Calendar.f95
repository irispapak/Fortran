program calendar

  ! It creates a calendar based on the years (start, end) given by the user.
  
  integer year_start, year_end, rows, cols
  
 !ask for start and end year
  write(6,*)'Give the start year:'
  read(5,*)year_start
  write(6,*)'Give the end year:'
  read(5,*)year_end

  call calendar_sub(year_start, year_end)
  
end program calendar

!-------------------------

subroutine calendar_sub(year_start, year_end)

  integer year_start, year_end, m, d, y, c, l, rows, i, j, cols
  integer, dimension(7) :: mon31=(/1,3,5,7,8,10,12/)
  integer, dimension(4) :: mon30=(/4,6,9,11/)
  integer, dimension(1) :: mon28=(/2/)
  integer, dimension (:,:), allocatable :: cal
  integer, dimension (:), allocatable :: leap, common

  !count leap and common years
  c=0
  l=0
  do y=year_start, year_end
     if (mod(y,4)/=0) then
        c=c+1
     else if (mod(y,100)/=0) then
        l=l+1
     else if (mod(y,400)/=0) then
        c=c+1
     else
        l=l+1
     end if
  end do

  !allocate the arrays
  rows=c*365+l*366
  cols=3
  allocate(cal(rows,cols))
  allocate(leap(l))
  allocate(common(c))
  !write(6,*)c,l

  !create new array with leap and common years
  i=1
  j=1
  do y=year_start, year_end
     if (mod(y,4)/=0) then
        common(i)=y
        i=i+1
     else if (mod(y,100)/=0) then
        leap(j)=y
        j=j+1
     else if (mod(y,400)/=0) then
        common(i)=y
        i=i+1
     else
        leap(j)=y
        j=j+1
     end if
  end do

  !write(6,*)leap

  !construct the cal array which contains all the dates 
  i=1
  do y=year_start, year_end
     do m=1,12
        if (any(m==mon31)) then
           do d=1,31
              cal(i,1)=d
              cal(i,2)=m
              cal(i,3)=y
              i=i+1
           end do
        else if (any(m==mon30)) then
           do d=1,30
              cal(i,1)=d
              cal(i,2)=m
              cal(i,3)=y
              i=i+1
           end do
        else if (any(m==mon28)) then
           do d=1,28
              cal(i,1)=d
              cal(i,2)=m
              cal(i,3)=y
              i=i+1
           end do
           if (any(y==leap)) then
              cal(i,1)=d
              cal(i,2)=m
              cal(i,3)=y
              i=i+1
           end if
        end if
     end do
  end do
  
  !write into file the cal array
  open(1,file='calendar.txt')
  do i=1,rows
     write(1,2)(cal(i,j),j=1,3)
  end do
  close(1)
 2 format(i4,i4,i7)        

  return
  
end subroutine calendar_sub
