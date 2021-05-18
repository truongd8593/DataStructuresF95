MODULE improved_linked_list
	USE linked_list, ONLY: node
	IMPLICIT NONE

	PRIVATE
	PUBLIC :: head, tail
	PUBLIC :: create_head, append_node

	TYPE (node), POINTER     :: head => null()
	TYPE (node), POINTER     :: tail => null()

	INTERFACE create_head
		MODULE PROCEDURE allocate_head_and_assign_value
	END INTERFACE
	
	INTERFACE append_node
		MODULE PROCEDURE allocate_and_assign_value
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

		SUBROUTINE allocate_and_assign_value(num)
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

END MODULE