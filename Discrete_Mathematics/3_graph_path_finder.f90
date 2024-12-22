PROGRAM QUES6
    IMPLICIT NONE
    INTEGER, ALLOCATABLE, DIMENSION(:,:) :: A, B
    INTEGER :: I, J, N, R

    ! Initialize values
    I = 1
    J = 4
    R = 5

    ! Open input file and read data
    OPEN(10, FILE = 'in6q6.txt')
    READ(10, *) N
    ALLOCATE(A(N, N), B(N, N))
    READ(10, *) ((A(I, J), J = 1, N), I = 1, N)
    CLOSE(10)

    ! Open output file and write adjacency matrix
    OPEN(UNIT = 11, FILE = "out6q6.txt")
    WRITE(11, '(" ADJACENCY MATRIX: ")')

    DO I = 1, N
        DO J = 1, N
            WRITE(11, '(I5, 2X)', ADVANCE = 'NO') A(I, J)
        END DO
        WRITE(11, '(/)')
    END DO

    ! Call matrix power subroutine and write result
    CALL MATPOWER(A, B, N, R)
    WRITE(11, '(/, I5, A, I0)') B(1, 4), " PATHS OF LENGTH ", R
    WRITE(11, '(A, I0, A, I0)') "BETWEEN THE VERTICES U", I, " AND U", J

    CLOSE(11)

END PROGRAM

SUBROUTINE MATPOWER(A, B, N, R)
    IMPLICIT NONE
    INTEGER :: I, J, K, N, R, T
    INTEGER :: A(N, N), B(N, N), C(N, N)

    ! Initialize matrix C to A
    C(:,:) = A(:,:)

    ! Matrix exponentiation
    DO T = 1, R - 1
        DO I = 1, N
            DO J = 1, N
                B(I, J) = 0
                DO K = 1, N
                    B(I, J) = B(I, J) + C(I, K) * A(K, J)
                END DO
            END DO
        END DO
        ! Update C to the new matrix B
        C(:,:) = B(:,:)
    END DO

END SUBROUTINE
