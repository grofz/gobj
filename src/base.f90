module base_mod
  implicit none
  private
  public pointobj, unpointobj, alloc_counter
 !protected alloc_counter

  type, public, abstract :: baseobj_t
    ! parent class for the actual object types
  end type baseobj_t

  type, public :: objcont_t
    ! container for the object
    integer :: nref = 0 ! how many times is it referenced
    integer :: id = 0   ! its index in primary array
    class(baseobj_t), pointer :: obj => null()
  end type objcont_t
  interface objcont_t
    module procedure :: new_objcont
  end interface

  type :: objcont_ptr
    ! structure to create array of containers
    type(objcont_t), pointer :: ptr => null()
  end type objcont_ptr

  type, public :: objlist_t
    ! dynamic array of pointers to objects
    type(objcont_ptr), allocatable :: arr(:)
    integer :: nuse = 0
    logical :: isprimarylist = .false.
  contains
    procedure newtolist, addtolist, remfromlist
    procedure getid, getobj
  end type objlist_t

  integer, parameter :: BASE_SIZE = 5
  integer :: alloc_counter = 0

contains

  ! ====================================
  ! Container for the object (objcont_t)
  ! ====================================
  function new_objcont(obj) result(new)
    class(baseobj_t), intent(in) :: obj
    type(objcont_t), pointer :: new
    
    allocate(new)
    allocate(new%obj, mold=obj)

    ! FOR DEBUGING
    alloc_counter = alloc_counter + 1
  end function new_objcont


  subroutine pointobj(other, this)
    type(objcont_t), pointer, intent(out) :: other
    type(objcont_t), intent(inout), target :: this

    other => this
    this%nref = this%nref + 1
  end subroutine pointobj


  subroutine unpointobj(this)
    type(objcont_t), intent(inout), pointer :: this

    if (.not. associated(this)) &
      & error stop 'unpointobj - pointer not associated'

    if (this%nref <= 0) then
      error stop 'unpointobj - nref value is zero'
    else if (this%nref == 1) then
      deallocate(this%obj)
      deallocate(this)

      ! FOR DEBUGING
      alloc_counter = alloc_counter - 1
    else
      this%nref = this%nref - 1
    end if
  end subroutine unpointobj


  ! ===============================================
  ! Dynamic array of pointers to object (objlist_t)
  ! ===============================================

  ! Create new object by adding it to the primary list...
  subroutine newtolist(this, new)
    class(objlist_t), intent(inout) :: this
    class(baseobj_t) :: new

    if (.not. this%isprimarylist) &
      & error stop 'newtolist - list must be a primary list'
    call addtolist0(this, objcont_t(new))
  end subroutine newtolist

  ! ... or add an existing object to a secondary list
  subroutine addtolist(this, what)
    class(objlist_t), intent(inout) :: this
    type(objcont_t), intent(in), pointer :: what

    if (this%isprimarylist) &
      & error stop 'addtolist - not allowed for primary lists'
    call addtolist0(this, what)
  end subroutine addtolist


  subroutine addtolist0(this, what)
    class(objlist_t), intent(inout) :: this
    type(objcont_t), intent(in), pointer :: what

    type(objcont_ptr), allocatable :: tmparr(:)

    if (.not. associated(what)) &
      & error stop 'addtolist - null pointer on input'

    if (.not. allocated(this%arr)) allocate(this%arr(BASE_SIZE))
    if (this%nuse==size(this%arr)) then
      allocate(tmparr(2*size(this%arr)))
      tmparr(1:size(this%arr)) = this%arr
      call move_alloc(tmparr, this%arr)
    end if
    this%nuse = this%nuse+1
    call pointobj(this%arr(this%nuse)%ptr, what)

    ! Enforce "id" corresponds to object's index in primary lists
    if (this%isprimarylist) what%id = this%nuse
  end subroutine addtolist0


  ! Remove item from the list
  subroutine remfromlist(this, index, ispermissive)
    class(objlist_t), intent(inout) :: this
    integer, intent(in) :: index
    logical, optional, intent(in) :: ispermissive

    logical :: ispermissive0

    ispermissive0 = .false.
    if (present(ispermissive)) ispermissive0 = ispermissive

    if (index>this%nuse .or. index<1) &
      & error stop 'remfromlist - index is out of bounds'

    if (.not. associated(this%arr(index)%ptr)) &
      & error stop 'remfromlist - null pointer unexpected'

    ! Removing from primary list not allowed if object is used
    if (this%isprimarylist .and. this%arr(index)%ptr%nref>1 &
        .and. .not. ispermissive0) &
      & error stop 'remfromlist - cannot remove an used object from primary list'

    call unpointobj(this%arr(index)%ptr)

    ! fill the "hole" in array by moving the last item
    if (index /= this%nuse) then
      this%arr(index) = this%arr(this%nuse)
      if (.not. associated(this%arr(index)%ptr)) &
        & error stop 'remfromlist - null pointer at the end unexpected'
      if (this%isprimarylist) &
        & this%arr(index)%ptr%id = index
    end if
    this%nuse = this%nuse - 1
  end subroutine remfromlist


  ! Find and Return pointer to the object container with a particular id
  function getid(this, id) result(obj)
    class(objlist_t), intent(in) :: this
    integer, intent(in) :: id
    type(objcont_t), pointer :: obj

    integer :: i

    obj => null()
    if (this%isprimarylist) then
      i = id
      if (i < 1 .or. i > this%nuse) &
        & error stop 'getid - id out of bounds'
      if (.not. associated(this%arr(i)%ptr)) &
        & error stop 'getid - a null pointer in primary list'
      if (this%arr(i)%ptr%id /= id) &
        & error stop 'getid - id is not same as index in primary list'
    else
      do i=1, this%nuse
        if (.not. associated(this%arr(i)%ptr)) &
          & error stop 'getid - encountered a null pointer'
        if (this%arr(i)%ptr%id==id) exit
      end do
      ! return a null pointer if not found
      if (i==this%nuse+1) return
    end if
    obj => this%arr(i)%ptr
  end function getid


  ! TODO
  function getobj(this, id) result(baseobj)
    class(objlist_t), intent(in) :: this
    integer, intent(in) :: id
    class(baseobj_t), pointer :: baseobj
    associate(objcont=>getid(this, id))
      baseobj => objcont%obj
    end associate
  end function getobj

end module base_mod