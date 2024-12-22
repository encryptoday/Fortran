PROGRAM QUES8
    IMPLICIT NONE
    INTEGER :: I, J, K, N
    INTEGER, DIMENSION(0:100, 100, 100) :: V

    ! Open input and output files
    OPEN(UNIT = 20, FILE = "in6q8.txt")
    OPEN(UNIT = 10, FILE = "out6q8.txt")

    ! Prompt user to enter the value of N
    PRINT *, "PLEASE ENTER THE VALUE OF N:"
    READ *, N

    ! Read values into array V
    READ(20, *) ((V(0, J, K), K = 1, N), J = 1, N)

    ! Perform operations to update V based on conditions
    DO I = 1, N
        DO J = 1, N
            DO K = 1, N
                IF ((V(I - 1, J, I) + V(I - 1, I, K)) < V(I - 1, J, K)) THEN
                    V(I, J, K) = (V(I - 1, J, I) + V(I - 1, I, K))
                ELSE
                    V(I, J, K) = V(I - 1, J, K)
                END IF
            END DO
        END DO
    END DO

    ! Write the results to the output file
    DO I = 0, N
        WRITE(10, '(A, I2)') "ITERATION NUMBER:", I
        DO J = 1, N
            DO K = 1, N
                WRITE(10, '(I5)', ADVANCE = "NO") V(I, J, K)
            END DO
            WRITE(10, *)
        END DO
        WRITE(10, *)
    END DO

    ! Close files
    CLOSE(UNIT = 20)
    CLOSE(UNIT = 10)

END PROGRAM
