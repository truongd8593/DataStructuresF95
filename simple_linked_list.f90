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
  
  INTEGER              :: num
  TYPE (node), POINTER :: tranverser => null()

  ! build up the list

  NULLIFY(list)                         ! initially nullify list (empty)

  DO num = 1, 5
	IF (num == 1) THEN 
		CALL create_head(num)
	ELSE
		CALL append_tail(num)
	ENDIF
  ENDDO

  PRINT *, 'Transverse the list built up and print the values'

  CALL print_list(head)

  PRINT *, 'Insert new node with value is 10 after node 3'
  
  tranverser => head
  DO 
	IF (.NOT. ASSOCIATED(tranverser)) EXIT
	IF (tranverser%value == 3) CALL append_after_node(tranverser, 10)
	tranverser => tranverser%next
  ENDDO

  PRINT *, 'Transverse the list built up and print the values'

  CALL print_list(head)

END PROGRAM simple_linked_list


