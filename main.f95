program sort
integer,allocatable:: a(:)
integer::n,temp,iptr

write(*,*)"How many numbers in list:"
read(*,*)n
allocate(a(n))

do i=1,n
    write(*,*)"Enter number:"
    read(*,*)a(i)
end do

do i=1,n-1
    iptr=i
    do j=i+1,n
        if(a(j)<a(iptr))then
            iptr=j
        end if
    end do

    if(i/=iptr)then
        temp=a(i)
        a(i)=a(iptr)
        a(iptr)=temp
    end if
end do

do i=1,n
    write(*,*)a(i)
end do


write(*,*)"Largest = ",a(n)
write(*,*)"Second Largest = ",a(n-1)
write(*,*)""
write(*,*)"Smallest = ",a(1)
write(*,*)"Second Smallest = ",a(2)

deallocate(a)
end program
