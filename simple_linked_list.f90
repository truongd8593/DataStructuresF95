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
!    Notice the linked list stores the typed-in numbers in 
!    reverse order.
!
!------------------------------------------------------------------------------

PROGRAM simple_linked_list
  USE linked_list
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

END PROGRAM simple_linked_list


