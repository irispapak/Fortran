program dateofbirth

  integer y, m, d, jul
    
  write(6,*)'Give me the date of your birth:'
  write(6,*)'Year='
  read(5,*)y
  write(6,*)'Month='
  read(5,*)m
  write(6,*)'Day='
  read(5,*)d

  call julian_day(y, m, d, jul)

  write(6,*)'Julian Day=',jul


end program dateofbirth

!------------------------

subroutine julian_day(year, month, day, julian)

  integer i, year, month, day, julian, sum
  integer, dimension(12) :: daysofmon=(/31,28,31,30,31,30,31,31,30,31,30,31/)

  sum=0
  do i=1,12
     if (month==i) then
        julian=day+sum
     end if
     sum=daysofmon(i)+sum
  end do


  !leap year condition
  if ((mod(year,4)==0 .and. mod(year,100)/=0) .or. mod(year,400)==0) then
     if (month>2 .or. (month==2 .and. day==29)) then
        julian=julian+1
     end if
  end if

  return

end subroutine julian_day
