SUBROUTINE list_selection_sort
	USE linked_list
	USE improved_linked_list
	IMPLICIT NONE
	TYPE (linked_list_t) :: linked_list_demo

	CALL create_head(linked_list_demo, 10)
	CALL append_tail(linked_list_demo, 8)
	CALL append_tail(linked_list_demo, 6)
	CALL append_tail(linked_list_demo, 1)
	CALL append_tail(linked_list_demo, 5)
	CALL append_tail(linked_list_demo, 2)
	CALL append_tail(linked_list_demo, 9)
	CALL append_tail(linked_list_demo, 4)
	CALL append_tail(linked_list_demo, 7)
	CALL append_tail(linked_list_demo, 3)

	PRINT *, 'Transverse the list built up and print the values before sorting:'

	CALL print_list(linked_list_demo)

	CALL selection_sort(linked_list_demo)

	PRINT *, 'Transverse the list built up and print the values after sorting:'

	CALL print_list(linked_list_demo)
END SUBROUTINE