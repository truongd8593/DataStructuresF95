!------------------------------------------------------------------------------
!
!  Program name:
!
!    simple_linked_list
!
!  Purpose:
!
!    This program demonstrates how to build and transverse a 
!    simple linked list by using pointers and dynamic allocation
!    facilities.
!
!    Notice the linked list implementation in module linked_list 
!    stores the typed-in numbers in reverse order, whereas
!    the linked list implementation in module improved_linked_list 
!    preserves the order of points read.
!
!------------------------------------------------------------------------------

PROGRAM simple_linked_list
  USE linked_list
  USE improved_linked_list
  IMPLICIT NONE
  
  INTEGER :: num
  

  ! build up the list

  NULLIFY(list)                         ! initially nullify list (empty)

  PRINT *, 'Type-in an integer number to build a linked list (0 to terminate)'

  DO
    READ *, num                         ! read num from keyboard 
    IF (num == 0) EXIT                  ! until 0 is entered
    CALL create_node(num)
  END DO

  ! transverse the list and print the values

  PRINT *, 'Transverse the list built up and print the values'

  CALL print_list(list)

  PRINT *, 'Type-in an integer number to build another linked list (0 to terminate)'

  READ *, num                           ! read num from keyboard 
  IF (num /= 0) then                    ! if 0 is entered, do nothing
    CALL create_head(num)

    DO                                  ! create rest of list
      READ *, num                       ! read num from keyboard 
      IF (num == 0) EXIT                ! until 0 is entered
      
	  CALL append_tail(num)
    END DO
  END IF

  PRINT *, 'Transverse the list built up and print the values'

  CALL print_list(head)

  NULLIFY(head, tail)

  PRINT *, 'Type-in an integer number to build another linked list (0 to terminate)'

  READ *, num                           ! read num from keyboard 
  IF (num /= 0) then                    ! if 0 is entered, do nothing
    CALL create_head(num)

    DO                                  ! create rest of list
      READ *, num                       ! read num from keyboard 
      IF (num == 0) EXIT                ! until 0 is entered
      
	  CALL append_head(num)
    END DO
  END IF

  PRINT *, 'Transverse the list built up and print the values'

  CALL print_list(head)

END PROGRAM simple_linked_list


