PROGRAM QUES5
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 4
    INTEGER :: I, J
    REAL :: S
    REAL, DIMENSION(N, N+1) :: A
    REAL, DIMENSION(N) :: ROOT

    ! Open input and output files
    OPEN(10, FILE='in3q5.txt')
    READ(10, *) ((A(I, J), J = 1, N+1), I = 1, N)
    CLOSE(10)

    OPEN(11, FILE='out3q5.txt')
    
    ! Write the augmented matrix to the output file
    WRITE(11, '("AUGMENTED MATRIX: ", /, 4(5F10.2, /, /))')
    WRITE(11, *) ((A(I, J), J = 1, N+1), I = 1, N)

    ! Gaussian elimination
    DO J = 1, N
        CALL PIVOT(N, A, J)
        DO I = J+1, N
            A(I, :) = A(I, :) - A(J, :) * A(I, J) / A(J, J)
        END DO
    END DO

    ! Write the matrix after Gaussian elimination
    WRITE(11, '("AFTER GAUSSIAN ELIMINATION: ", /, 4(5F10.2, /, /))')
    WRITE(11, *) ((A(I, J), J = 1, N+1), I = 1, N)

    ! Back substitution to compute the roots
    DO I = N, 1, -1
        S = A(I, N+1)
        DO J = I+1, N
            S = S - A(I, J) * ROOT(J)
        END DO
        ROOT(I) = S / A(I, I)
    END DO

    ! Write the roots to the output file
    WRITE(11, '("ROOTS ARE: ", 4F8.2)') (ROOT(I), I = 1, N)
    CLOSE(11)
END PROGRAM QUES5

SUBROUTINE PIVOT(N, A, J)
    IMPLICIT NONE
    INTEGER :: N, I, J, K
    REAL, DIMENSION(N, N+1) :: A
    REAL, DIMENSION(N+1) :: T

    K = J
    DO I = J, N
        IF (ABS(A(I, J)) > ABS(A(K, J))) K = I
    END DO

    ! Swap rows J and K
    T = A(J, :)
    A(J, :) = A(K, :)
    A(K, :) = T
END SUBROUTINE PIVOT
