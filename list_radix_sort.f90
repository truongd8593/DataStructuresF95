SUBROUTINE list_radix_sort
	USE linked_list
	USE improved_linked_list
	IMPLICIT NONE
	TYPE (linked_list_t) :: singly_linked_list

	CALL create_head(singly_linked_list, 4)
	CALL append_tail(singly_linked_list, 6)
	CALL append_tail(singly_linked_list, 5)
	CALL append_tail(singly_linked_list, 1)
	CALL append_tail(singly_linked_list, 8)
	CALL append_tail(singly_linked_list, 2)

	PRINT *, 'Transverse the list built up and print the values before sorting:'

	CALL print_list(singly_linked_list)

	CALL radix_sort(singly_linked_list)

	PRINT *, 'Transverse the list built up and print the values after sorting:'

	CALL print_list(singly_linked_list)
END SUBROUTINE