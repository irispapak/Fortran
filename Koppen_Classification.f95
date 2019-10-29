program Koppen_Classification

  !Koppen Climate Classification
  character (len=15) :: name
  logical s, w, e
  integer i, j
  real temp(12), prec(12), r, t, rw, rs, r0, tc, th, rd, rl, height

  !read monthly data for temperature and precipitation
  write(6,*)'Give the name of the station:'
  read(5,*)name
  write(6,*)'Give the mean monthly temperatures:'
  do i=1,12
     read(5,*)temp(i)    
  end do
  write(6,*)'Give the mean monthly precipitation amounts:'
  do i=1,12
     read(5,*)prec(i)
  end do

  t=sum(temp(1:12))/12                       !mean annual temperature
  r=sum(prec(1:12))                          !annual precipitation

  rw=sum(prec(10:12))+sum(prec(1:3))         !winter precipitation (Oct-Mar)
  rs=sum(prec(4:9))                          !summer precipitation (Apr-Sep)

  if (rs>=0.7*r) then
     !it rains in summer
     r0=20*t+280
     s=.true.
     w=.false.
     e=.false.
  elseif (rw>=0.7*r) then
     !it rains in winter
     r0=20*t
     w=.true.
     s=.false.
     e=.false.
  else
     !equal distribution of rain
     r0=20*t+140
     w=.false.
     s=.false.
     e=.true.
  end if

  if (r<r0/2) then
     if (t>18) then
        write(6,*)'Bwh'
     else
        write(6,*)'Bwk'
     end if
  elseif (r0/2<r .and. r<r0) then
     if (t>18) then
        write(6,*)'Bsh'
     else
        write(6,*)'Bsk'
     end if
  else
     !find temperature of the coldest month
     tc=temp(1)
     do i=1,11
        if (temp(i+1)<tc) then
           tc=temp(i+1)
        end if
     end do

     !find the temperature of the hottest month
     th=temp(1)
     do i=1,11
        if (temp(i+1)>th) then
           th=temp(i+1)
        end if
     end do

     !find the precipitation of the driest month
     rd=prec(1)
     do i=1,11
        if (prec(i+1)<rd) then
           rd=prec(i+1)
        end if
     end do

     !find the precipitation of the wettest month (wet->liquid)
     rl=prec(1)
     do i=1,11
        if (prec(i+1)>rl) then
           rl=prec(i+1)
        end if
     end do
     
     if (tc>=18) then
        if(rd>=60) then
           write(6,*)'Af'
        elseif (100-(r/25)<=rd .and. rd<60) then
           write(6,*)'Am'
        elseif (rd<100-(r/25)) then
           write(6,*)'Aw'
        end if
     elseif (th>10 .and. (tc<18 .and. tc>-3)) then
        if (s) then
           if (rd<rl/10) then
              if (th>=22) then
                 write(6,*)'Cwa'
              else
                 write(6,*)'Cwb'
              end if
           else
              e=.true.
           end if
        end if
        
        if (w) then
           if (rd<30 .and. rd<rl/3) then
              if (th>=22) then
                 write(6,*)'Csa'
              else
                 write(6,*)'Csb'
              end if
           else
              e=.true.
           end if
        end if
        
        if (e) then
           if (th>=22) then
              write(6,*)'Cfa'
           else
              !sort temperatures from hotter to colder
              !find temperatures of 4 hottest months
              do i=1,11
                 do j=1,11
                    if (temp(j+1)>temp(j)) then
                       a=temp(j)
                       temp(j)=temp(j+1)
                       temp(j+1)=a
                    end if
                 end do
              end do
                   
              if (temp(1)>=10 .and. temp(2)>=10 .and. temp(3)>=10 .and. temp(4)>=10) then
                 write(6,*)'Cfb'
              else
                 write(6,*)'Cfc'
              end if
           end if
        end if
        
     elseif (th>10 .and. tc<=-3) then
        !sort temperatures from hotter to colder
        !find temperatures of 4 hottest months
        do i=1,11
           do j=1,11
              if (temp(j+1)>temp(j)) then
                 a=temp(j)
                 temp(j)=temp(j+1)
                 temp(j+1)=a
              end if
           end do
        end do
        
        if (s) then
           if (rd<rl/10) then
              if (th>=22) then
                 write(6,*)'Dwa'
              elseif (th<22 .and. tc>=-38) then
                 if (temp(1)>=10 .and. temp(2)>=10 .and. temp(3)>=10 .and. temp(4)>=10) then
                    write(6,*)'Dwb'
                 else
                    write(6,*)'Dwc'
                 end if
              elseif (tc<-38) then
                 write(6,*)'Dwd'
              end if
           else
              e=.true.
           end if
        end if
        
        if (e .or. w) then
           if (th>=22) then
              write(6,*)'Dfa'
           elseif (th<22 .and. tc>=-38) then
              if (temp(1)>=10 .and. temp(2)>=10 .and. temp(3)>=10 .and. temp(4)>=10) then
                 write(6,*)'Dfb'
              else
                 write(6,*)'Dfc'
              end if
           elseif (tc<-38) then
              write(6,*)'Dfd'
           end if
        end if
        
     elseif (th<10) then
        read(5,*)height
        if (height>=1500) then
           write(6,*)'H'
        else
           if (th>0) then
              write(6,*)'ET'
           else
              write(6,*)'EF'
           end if
        end if
     end if
  end if
  

end program Koppen_Classification
