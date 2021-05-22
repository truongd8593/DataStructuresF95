SUBROUTINE list_selection_sort
	USE linked_list
	USE improved_linked_list
	IMPLICIT NONE
	TYPE (node), POINTER :: tranverser => null()

	CALL create_head(10)
	CALL append_tail(8)
	CALL append_tail(6)
	CALL append_tail(1)
	CALL append_tail(5)
	CALL append_tail(2)
	CALL append_tail(9)
	CALL append_tail(4)
	CALL append_tail(7)
	CALL append_tail(3)

	PRINT *, 'Transverse the list built up and print the values before sorting:'

	CALL print_list(head)

	CALL selection_sort(head)

	PRINT *, 'Transverse the list built up and print the values after sorting:'

	CALL print_list(head)
END SUBROUTINE