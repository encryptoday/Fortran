PROGRAM QUES3
    IMPLICIT NONE
    INTEGER :: I, J, N
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: ARR1, ARR2, ARR3, ARR4

    ! Prompt user for N, ensuring N >= 3
10  READ(*,*) N
    IF (N < 3) GOTO 10

    ! Allocate memory for matrices
    ALLOCATE(ARR1(N, N), ARR2(N, N), ARR3(N, N), ARR4(N, N))

    ! Read input matrices from file
    OPEN(UNIT=10, FILE="in3q3.txt")
    READ(10, *) ((ARR1(I, J), J = 1, N), I = 1, N), ((ARR2(I, J), J = 1, N), I = 1, N)
    CLOSE(10)

    ! Open output file
    OPEN(UNIT=10, FILE="out3q3.txt")

    ! Write the input matrices to the file
    WRITE(10, '("MATRIX M IS:", /, (3I5, /))') ((ARR1(I, J), J = 1, N), I = 1, N)
    WRITE(10, '("MATRIX N IS:", /, (3I5, /))') ((ARR2(I, J), J = 1, N), I = 1, N)

    ! Perform matrix multiplication
    CALL MULTI(ARR1, ARR2, ARR3, N)
    CALL MULTI(ARR2, ARR1, ARR4, N)

    ! Write the results of the multiplications
    WRITE(10, '("MATRIX M*N IS:", /, (3I5, /))') ((ARR3(I, J), J = 1, N), I = 1, N)
    WRITE(10, '("MATRIX N*M IS:", /, (3I5, /))') ((ARR4(I, J), J = 1, N), I = 1, N)

    ! Check if M*N equals N*M (commutativity)
    DO I = 1, N
        DO J = 1, N
            IF (ARR3(I, J) /= ARR4(I, J)) THEN
                WRITE(10, *) "M, N DO NOT COMMUTE"
                GOTO 100
            END IF
        END DO
    END DO

    WRITE(10, *) "M, N COMMUTE"

100 CLOSE(10)
END PROGRAM

SUBROUTINE MULTI(A, B, C, N)
    INTEGER, DIMENSION(N, N) :: A, B, C
    INTEGER :: N, I, J, K

    ! Initialize the result matrix to zero
    C = 0

    ! Perform matrix multiplication
    DO I = 1, N
        DO J = 1, N
            DO K = 1, N
                C(I, J) = C(I, J) + A(I, K) * B(K, J)
            END DO
        END DO
    END DO
END SUBROUTINE
