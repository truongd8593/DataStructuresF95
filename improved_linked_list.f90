MODULE improved_linked_list
	USE linked_list, ONLY: node
	IMPLICIT NONE

	PRIVATE
	PUBLIC :: head, tail
	PUBLIC :: create_head, append_tail, append_head, append_after_node

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

END MODULE