PROGRAM QUES5
    IMPLICIT NONE
    INTEGER :: N, I
    LOGICAL :: PRIME

    ! Read the value of N
    READ(*,*) N

    ! Open the output file
    OPEN(UNIT=10, FILE="out2q5.txt")
    WRITE(10, *) "VALUE OF N IS: ", N
    WRITE(10, *) "THE PRIME DIVISORS ARE:"

    ! Check for prime divisors of N
    DO I = 2, N
        IF (MOD(N, I) == 0) THEN
            CALL CHECK_PRIME(I, PRIME)
            IF (PRIME) WRITE(10, *) I
        END IF
    END DO

    ! Close the output file
    CLOSE(10)
END PROGRAM

! Subroutine to check if a number is prime
SUBROUTINE CHECK_PRIME(N, Q)
    INTEGER :: N, I
    LOGICAL :: Q

    Q = .TRUE.

    ! Loop to check divisors of N
    DO I = 2, N / 2
        IF (MOD(N, I) == 0) THEN
            Q = .FALSE.
            EXIT
        END IF
    END DO

    RETURN
END SUBROUTINE
