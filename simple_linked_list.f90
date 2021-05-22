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
  IMPLICIT NONE
  INTEGER :: option
  
  PRINT*,'Please selection one option.'
  PRINT*,'Option 1: Demo create a linked list, insert a new node to that list, remove that node'
  PRINT*,'Option 2: Demo sorting a linked list by selection sort algorithm'

  READ*, option
  IF (option == 1) CALL list_create_insert_remove
  IF (option == 2) CALL list_selection_sort

END PROGRAM simple_linked_list


