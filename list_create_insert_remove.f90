SUBROUTINE list_create_insert_remove
	USE linked_list
	USE improved_linked_list
	IMPLICIT NONE
	INTEGER              :: num
	TYPE (linked_list_t) :: linked_list_demo

	DO num = 1, 5
		IF (num == 1) THEN 
			CALL create_head(linked_list_demo, num)
		ELSE
			CALL append_tail(linked_list_demo,num)
		ENDIF
	ENDDO

	PRINT *, 'Transverse the list built up and print the values'

	CALL print_list(linked_list_demo)

	PRINT *, 'Insert new node with value is 10 after node 3 ...'

	CALL append_after_node(linked_list_demo, 3, 10)

	PRINT *, 'Transverse the list built up and print the values'

	CALL print_list(linked_list_demo)

	PRINT *, 'Remove node has value 10 ...'

	CALL remove_node(linked_list_demo, 10)

	PRINT *, 'Transverse the list built up and print the values'

	CALL print_list(linked_list_demo)

END SUBROUTINE