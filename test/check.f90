program check
  use base_mod
  implicit none

  type, extends(baseobj_t) :: test_t
    integer :: val
    type(objlist_t) :: list
  end type

  type(objlist_t) :: primlist, seconlist
  type(objcont_t), pointer :: obj, obj2
  type(test_t) :: mold
  class(baseobj_t), pointer :: baseobj

  ! add number 1 and number 2
  primlist%isprimarylist = .true.
  call primlist%newtolist(mold)
  call primlist%newtolist(mold)

  ! add number 1 to number 2's private list
  obj=>primlist%getid(2)
  baseobj=>primlist%getobj(2)
  select type(objx=>obj%obj)
    type is (test_t)
      call objx%list%addtolist(primlist%arr(1)%ptr)
      objx%val = 20
    class default
      error stop 'unexpected type'
  end select

  !select type(baseobj)
  select type(a=>obj%obj)
    type is (test_t)
      !baseobj%val = 21
      a%val = 21
  end select


  obj=>primlist%getid(1)
  select type(objx=>obj%obj)
    type is (test_t)
      objx%val = 10
    class default
      error stop 'unexpected type'
  end select

  call print_primlist

  ! now remove number 1 from primary list
  call primlist%remfromlist(1, ispermissive=.true.)
  call print_primlist

  ! ... but check, that number 1 still exists in the reference
  obj => primlist%getid(1)
  select type(objx=>obj%obj)
  type is (test_t)
    obj2 => objx%list%getid(1)
    select type(objxx=>obj2%obj)
    type is (test_t)
      print *, 'val = ', objxx%val
    end select
    call objx%list%remfromlist(1)
  class default
    error stop 'unexpected type'
  end select
  call print_primlist

  call primlist%remfromlist(1)
  print *, 'alloc_counter =', alloc_counter


  
contains
  subroutine print_primlist
    integer :: i
    print *,'id, nref, val, list_size'
    do i=1, primlist%nuse
      obj=>primlist%getid(i)
      select type (objx=>obj%obj)
      type is (test_t)
        print *, primlist%arr(i)%ptr%id, primlist%arr(i)%ptr%nref, objx%val, objx%list%nuse
      end select
    end do
    print *, 'alloc_counter =', alloc_counter
  end subroutine


end program check
