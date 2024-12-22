PROGRAM QUES1
    IMPLICIT NONE
    INTEGER :: I, J, N
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: ARR, ARR2
    LOGICAL :: SYMMETRIC = .TRUE.

    ! Prompt user to enter N, ensuring N >= 2
10  READ(*,*) N
    IF (N < 2) GOTO 10

    ! Allocate arrays based on the input size
    ALLOCATE(ARR(N, N), ARR2(N, N))

    ! Read the elements of the matrix
    DO I = 1, N
        DO J = 1, N
            READ(*,*) ARR(I, J)
        END DO
    END DO

    ! Compute the transpose of the matrix
    ARR2 = TRANSPOSE(ARR)

    ! Open output file
    OPEN(UNIT=10, FILE="out3q1.txt")

    ! Write the original matrix to the file
    WRITE(10, *) "MATRIX IS:"
    DO I = 1, N
        DO J = 1, N
            WRITE(10, '(I3)', ADVANCE='NO') ARR(I, J)
        END DO
        WRITE(10, *)
    END DO

    ! Write the transpose matrix to the file
    WRITE(10, *) "TRANSPOSE MATRIX IS:"
    DO I = 1, N
        DO J = 1, N
            WRITE(10, '(I3)', ADVANCE='NO') ARR2(I, J)
        END DO
        WRITE(10, *)
    END DO

    ! Check if the matrix is symmetric
    DO I = 1, N
        DO J = 1, N
            IF (ARR(I, J) /= ARR(J, I)) THEN
                SYMMETRIC = .FALSE.
                EXIT
            END IF
        END DO
        IF (.NOT. SYMMETRIC) EXIT
    END DO

    ! Write whether the matrix is symmetric
    IF (SYMMETRIC) THEN
        WRITE(10, *) "MATRIX IS SYMMETRIC"
    ELSE
        WRITE(10, *) "MATRIX IS NOT SYMMETRIC"
    END IF

    ! Close the file
    CLOSE(10)
END PROGRAM