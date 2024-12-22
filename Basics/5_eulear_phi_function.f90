PROGRAM ques4
    IMPLICIT NONE
    INTEGER :: GCD, N, K, COUNTA

    COUNTA = 0

    ! Read the value of N
    READ(*,*) N

    ! Loop to calculate Euler's Totient Function (phi)
    DO K = 1, N
        IF (GCD(N, K) == 1) THEN
            COUNTA = COUNTA + 1
        END IF
    END DO

    ! Open output file for writing results
    OPEN(UNIT=10, FILE="out2q4.txt")
    WRITE(10, *) "VALUE OF N IS : ", N
    WRITE(10, *) "VALUE OF PHI(N) : ", COUNTA

END PROGRAM

! Recursive function to calculate GCD
RECURSIVE FUNCTION GCD(A, B) RESULT(ANSWER)
    INTEGER :: A, B, ANSWER

    IF (MOD(A, B) == 0) THEN
        ANSWER = B
    ELSE
        ANSWER = GCD(B, MOD(A, B))
    END IF
END FUNCTION
