MODULE improved_linked_list
	USE linked_list, ONLY: node
	USE generic_swap
	IMPLICIT NONE

	PRIVATE
	PUBLIC :: linked_list_t
	PUBLIC :: print_list
	PUBLIC :: create_head, append_tail, append_head, append_after_node
	PUBLIC :: remove_node
	PUBLIC :: selection_sort, selection_sort_2, quick_sort

	TYPE :: linked_list_t
		TYPE (node), POINTER    :: head => null()
		TYPE (node), POINTER    :: tail => null()
	END TYPE linked_list_t

	INTERFACE create_head
		MODULE PROCEDURE allocate_head_and_assign_value
	END INTERFACE
	
	INTERFACE append_tail
		MODULE PROCEDURE append_node_at_tail
	END INTERFACE

	INTERFACE append_head
		MODULE PROCEDURE append_node_at_head
	END INTERFACE

	INTERFACE append_after_node
		MODULE PROCEDURE append_new_node_after_node
	END INTERFACE

	INTERFACE remove_node
		MODULE PROCEDURE remove_node_has_key
	END INTERFACE

	INTERFACE selection_sort
		MODULE PROCEDURE list_selection_sort
	END INTERFACE

	INTERFACE selection_sort_2
		MODULE PROCEDURE list_selection_sort_2
	END INTERFACE

	INTERFACE quick_sort
		MODULE PROCEDURE list_quick_sort
	END INTERFACE

	INTERFACE print_list
		MODULE PROCEDURE tranverse_and_print
	END INTERFACE

	CONTAINS

		SUBROUTINE allocate_head_and_assign_value(list, num)
			IMPLICIT NONE
			INTEGER, INTENT(in)  :: num
			INTEGER              :: status
			TYPE (linked_list_t) :: list

			ALLOCATE(list%head, STAT = status)       ! create the head of the list
			IF (status > 0) STOP 'Fail to allocate a new node'
			list%head%value = num                    ! give the value
			NULLIFY(list%head%next)                  ! point to null
			list%tail => list%head                   ! update tail of list

		END SUBROUTINE

		SUBROUTINE append_node_at_tail(list, num)
			IMPLICIT NONE
			INTEGER, INTENT(in)  :: num
			INTEGER              :: status
			TYPE (node), POINTER :: current
			TYPE (linked_list_t) :: list

			ALLOCATE(current, STAT = status)  ! create new node
			
			IF (status > 0) STOP 'Fail to allocate a new node'
			
			current%value = num               ! giving the value
			NULLIFY(current%next)             ! point to null (end of list)
			list%tail%next => current         ! link to tail of list
			list%tail => current              ! update tail of list

		END SUBROUTINE

		SUBROUTINE append_node_at_head(list, num)
			IMPLICIT NONE
			INTEGER, INTENT(in)  :: num
			INTEGER              :: status
			TYPE (node), POINTER :: current
			TYPE (linked_list_t) :: list

			ALLOCATE(current, STAT = status)  ! create new node

			IF (status > 0) STOP 'Fail to allocate a new node'
			
			current%value = num               ! giving the value
			NULLIFY(current%next)             ! point to null (end of list)
			current%next => list%head    ! link to head of list
			list%head => current              ! update head of list
		END SUBROUTINE

		SUBROUTINE append_new_node_after_node(list, num, value)
			IMPLICIT NONE
			INTEGER, INTENT(in)  :: num, value
			INTEGER              :: status
			TYPE (node), POINTER :: p
			TYPE (node), POINTER :: q
			TYPE (node), POINTER :: current
			TYPE (linked_list_t) :: list

			p => list%head

			DO WHILE (ASSOCIATED(p))
				q => p
				IF (q%value == num) THEN
					ALLOCATE(current, STAT = status)  ! create new node
					IF (status > 0) STOP 'Fail to allocate a new node'

					current%value = value               ! giving the value
					current%next => q%next
					q%next => current

					IF (ASSOCIATED(q, list%tail)) list%tail => current
				ENDIF
				p => p%next
			ENDDO

		END SUBROUTINE

		SUBROUTINE remove_node_has_key(list, num)
			IMPLICIT NONE
			INTEGER, INTENT(in)        :: num
			TYPE (node), POINTER       :: p, q
			TYPE (linked_list_t)       :: list

			p => list%head
			q => null()

			DO WHILE (ASSOCIATED(p))
				IF (p%value == num) EXIT
				q => p
				p => p%next
			ENDDO

			IF (.NOT. ASSOCIATED(p)) THEN
				PRINT*, 'Cannot find node to remove'
				RETURN
			ENDIF

			IF (ASSOCIATED(q)) THEN
				IF (ASSOCIATED(p, list%tail)) THEN
					list%tail => q
				ENDIF
				q%next => p%next
				NULLIFY(p)
			ELSE
				list%head => p%next
				IF (.NOT. ASSOCIATED(list%head)) NULLIFY(list%tail)
			ENDIF
		END SUBROUTINE

		RECURSIVE SUBROUTINE list_quick_sort(l)
			IMPLICIT NONE
			TYPE (linked_list_t) :: l, l1, l2
			TYPE (node), POINTER :: p, q

			IF (ASSOCIATED(l%head, l%tail)) RETURN

			q => l%head
			l%head => q%next

			DO WHILE (ASSOCIATED(l%head))
				p => l%head
				l%head => p%next
				p%next => null()

				IF (p%value <= q%value) THEN
					CALL add_tail(l1, p)
				ELSE
					CALL add_tail(l2, p)
				ENDIF
			ENDDO

			CALL list_quick_sort(l1)
			CALL list_quick_sort(l2)

			IF (ASSOCIATED(l1%head)) THEN
				l%head => l1%head
				l1%tail%next => q
			ELSE
				l%head => q
			ENDIF

			q%next => l2%head

			IF (ASSOCIATED(l2%head)) THEN
				l%tail => l2%tail
			ELSE
				l%tail => q
			ENDIF

		END SUBROUTINE

		SUBROUTINE list_selection_sort(list)
			! Selection sort algorithm on linked list by manipulating data field
			IMPLICIT NONE
			TYPE (linked_list_t) :: list
			TYPE (node), POINTER :: p, q, min

			p => list%head

			DO WHILE (ASSOCIATED(p))
				q => p%next
				min => p

				DO WHILE (ASSOCIATED(q))
					IF (q%value < min%value) THEN
						min => q
					ENDIF
					q => q%next
				ENDDO 

				CALL swap(min%value, p%value)
				p => p%next
			ENDDO
		END SUBROUTINE

		SUBROUTINE list_selection_sort_2(list)
			IMPLICIT NONE
			! Selection sort algorithm on linked list by manipulating pointer 'next'
			TYPE (linked_list_t) :: list
			TYPE (linked_list_t) :: list_res
			TYPE (node), POINTER :: min, min_prev, p, q

			DO WHILE (ASSOCIATED(list%head))
				p => list%head
				q => p%next
				min => p
				min_prev => null()

				DO WHILE (ASSOCIATED(q))
					IF (q%value < min%value) THEN
						min => q
						min_prev => p
					ENDIF
					p => q
					q => q%next
				ENDDO

				IF (ASSOCIATED(min_prev)) THEN
					min_prev%next => min%next
				ELSE
					list%head => min%next
				ENDIF

				! Append min to tail of list_res
				CALL add_tail(list_res, min)

			ENDDO

			! Assign list = list_res
			list%head => list_res%head
			list%tail => list_res%tail

		END SUBROUTINE

		SUBROUTINE add_tail(l, new_node)
			IMPLICIT NONE
			TYPE (linked_list_t) :: l
			TYPE (node), POINTER :: new_node

			IF (.NOT. ASSOCIATED(l%head)) THEN
				l%head => new_node
				l%tail => l%head
			ELSE
				l%tail%next => new_node
				l%tail => new_node
			ENDIF
		END SUBROUTINE

		SUBROUTINE tranverse_and_print(list)
			IMPLICIT NONE
			TYPE (linked_list_t) :: list
			TYPE (node), POINTER :: current

			current => list%head                    ! make current as alias of list

			DO
				IF (.NOT. ASSOCIATED(current)) EXIT ! exit if null pointer
				PRINT *, current%value              ! print the value
				current => current%next             ! make current alias of next node
			END DO

		END SUBROUTINE

END MODULE