PROGRAM ques4
    IMPLICIT NONE
    INTEGER :: I, J, K, M, N, TAG = 0
    DOUBLE PRECISION :: S, W = 1.2, TOL = 0.001
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: A, X
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: EA

    ! Open input file
    OPEN(1001, FILE='in5q4.txt')
    READ(1001,*) N, M
    ALLOCATE(X(0:M, N), A(N, N+1), EA(N))
    READ(1001,*) ((A(I, J), J = 1, N+1), I = 1, N)
    READ(1001,*) (X(0, J), J = 1, N)
    CLOSE(1001)

    ! Open output file
    OPEN(UNIT=11, FILE="out5q4.txt")
    WRITE(11, '(//, "THE COEFFICIENT MATRIX:(A|B) ")')

    ! Output the coefficient matrix (A|B)
    DO I = 1, N
        DO J = 1, N+1
            WRITE(11, '(F10.2, 2X)', ADVANCE='NO') A(I, J)
        END DO
        WRITE(11, *)
    END DO

    ! Perform pivoting
    CALL PIVOT(A, N)
    WRITE(11, *) "AFTER PIVOTING: "

    ! Output the matrix after pivoting
    DO I = 1, N
        DO J = 1, N+1
            WRITE(11, '(F10.2, 2X)', ADVANCE='NO') A(I, J)
        END DO
        WRITE(11, *)
    END DO

    ! Output the values of N
    WRITE(11, *)
    WRITE(11, '(A)', ADVANCE='NO') "N"
    WRITE(11, *)
    DO I = 1, N
        WRITE(11, '(A, I2, A)', ADVANCE='NO') "X(", I, ")"
    END DO
    WRITE(11, *)

    ! Perform iterative calculation
    DO K = 1, M
        EA(:) = 0  ! Absolute error matrix
        WRITE(11, '(I4)', ADVANCE='NO') K
        DO I = 1, N
            S = 0
            DO J = 1, N
                IF (J < I) THEN
                    S = S + A(I, J) * X(K, J)
                ELSE IF (J > I) THEN
                    S = S + A(I, J) * X(K-1, J)
                END IF
            END DO
            X(K, I) = (1 - W) * X(K-1, I) + (W / A(I, I)) * (A(I, N+1) - S)
            EA(I) = ABS(X(K, I) - X(K-1, I))
            WRITE(11, '(F20.7, 2X)', ADVANCE='NO') X(K, I)
        END DO
        WRITE(11, *)

        ! Check for convergence
        IF (MAXVAL(EA) < TOL) THEN
            DO I = 1, N
                WRITE(11, '(A, I2, A, F20.7)') "X(", I, ")=", X(K, I)
            END DO
            TAG = 1
            EXIT
        END IF
    END DO

    ! If maximum iterations exceeded
    WRITE(11, *)
    IF (TAG == 0) WRITE(11, *) "MAXIMUM NUMBER OF ITERATIONS EXCEEDED"
    
    CLOSE(11)
    
END PROGRAM

SUBROUTINE PIVOT(A, N)
    INTEGER :: I, J, N
    DOUBLE PRECISION :: A(N, N+1), T(N+1)

    ! Pivoting process
    DO I = 1, N
        DO J = 1, N
            IF (ABS(A(I, I)) < ABS(A(J, I))) THEN
                T(:) = A(I, :)
                A(I, :) = A(J, :)
                A(J, :) = T(:)
            END IF
        END DO
    END DO
END SUBROUTINE
