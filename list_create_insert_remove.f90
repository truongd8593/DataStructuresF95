SUBROUTINE list_create_insert_remove
	USE linked_list
	USE improved_linked_list
	IMPLICIT NONE
	INTEGER              :: num
	TYPE (node), POINTER :: tranverser => null()

	DO num = 1, 5
		IF (num == 1) THEN 
			CALL create_head(num)
		ELSE
			CALL append_tail(num)
		ENDIF
	ENDDO

	PRINT *, 'Transverse the list built up and print the values'

	CALL print_list(head)

	PRINT *, 'Insert new node with value is 10 after node 3 ...'
  
	tranverser => head
	DO 
		IF (.NOT. ASSOCIATED(tranverser)) EXIT
		IF (tranverser%value == 3) CALL append_after_node(tranverser, 10)
		tranverser => tranverser%next
	ENDDO

	PRINT *, 'Transverse the list built up and print the values'

	CALL print_list(head)

	PRINT *, 'Remove node has value 10 ...'

	CALL remove_node(10)

	PRINT *, 'Transverse the list built up and print the values'

	CALL print_list(head)

END SUBROUTINE