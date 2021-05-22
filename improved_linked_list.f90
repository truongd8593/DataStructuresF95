MODULE improved_linked_list
	USE linked_list, ONLY: node
	USE generic_swap
	IMPLICIT NONE

	PRIVATE
	PUBLIC :: head, tail
	PUBLIC :: create_head, append_tail, append_head, append_after_node
	PUBLIC :: remove_node
	PUBLIC :: selection_sort

	TYPE (node), POINTER    :: head => null()
	TYPE (node), POINTER    :: tail => null()

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

	CONTAINS

		SUBROUTINE allocate_head_and_assign_value(num)
			IMPLICIT NONE
			INTEGER, INTENT(in)  :: num
			INTEGER              :: status

			ALLOCATE(head, STAT = status)       ! create the head of the list
			IF (status > 0) STOP 'Fail to allocate a new node'
			head%value = num                    ! give the value
			NULLIFY(head%next)                  ! point to null
			tail => head                        ! update tail of list

		END SUBROUTINE

		SUBROUTINE append_node_at_tail(num)
			IMPLICIT NONE
			INTEGER, INTENT(in)  :: num
			INTEGER              :: status
			TYPE (node), POINTER :: current

			ALLOCATE(current, STAT = status)  ! create new node
			
			IF (status > 0) STOP 'Fail to allocate a new node'
			
			current%value = num               ! giving the value
			NULLIFY(current%next)             ! point to null (end of list)
			tail%next => current              ! link to tail of list
			tail => current                   ! update tail of list

		END SUBROUTINE

		SUBROUTINE append_node_at_head(num)
			IMPLICIT NONE
			INTEGER, INTENT(in)  :: num
			INTEGER              :: status
			TYPE (node), POINTER :: current

			ALLOCATE(current, STAT = status)  ! create new node

			IF (status > 0) STOP 'Fail to allocate a new node'
			
			current%value = num               ! giving the value
			NULLIFY(current%next)             ! point to null (end of list)
			current%next => head              ! link to head of list
			head => current                   ! update head of list
		END SUBROUTINE

		SUBROUTINE append_new_node_after_node(q, num)
			IMPLICIT NONE
			INTEGER, INTENT(in)  :: num
			INTEGER              :: status
			TYPE (node), POINTER :: q
			TYPE (node), POINTER :: current


			IF (ASSOCIATED(q)) THEN			
				ALLOCATE(current, STAT = status)  ! create new node
				IF (status > 0) STOP 'Fail to allocate a new node'
			
				current%value = num               ! giving the value
				current%next => q%next
				q%next => current
			
				IF (ASSOCIATED(q, tail)) tail => current
			ENDIF

			IF (.NOT. ASSOCIATED(q)) THEN
				CALL append_head(num)
			ENDIF

		END SUBROUTINE

		SUBROUTINE remove_node_has_key(num)
			IMPLICIT NONE
			INTEGER, INTENT(in)  :: num
			TYPE (node), POINTER :: p, q

			p => head
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
				IF (ASSOCIATED(p, tail)) THEN
					tail => q
				ENDIF
				q%next => p%next
				NULLIFY(p)
			ELSE
				head => p%next
				IF (.NOT. ASSOCIATED(head)) NULLIFY(tail)
			ENDIF
		END SUBROUTINE

		SUBROUTINE list_selection_sort(list)
			IMPLICIT NONE
			TYPE (node), POINTER :: list
			TYPE (node), POINTER :: p, q, min

			p => list

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

END MODULE