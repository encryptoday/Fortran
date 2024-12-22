PROGRAM QUES6
    IMPLICIT NONE

    INTEGER :: I, N
    REAL :: F, I3, A, B, X, S, H
    REAL, PARAMETER :: PI = 3.1416

    ! Initialization
    A = 0
    B = PI / 4
    N = 36
    H = (B - A) / N
    S = 0

    ! Simpson's 3/8 Rule
    DO I = 1, N - 1
        IF (MOD(I, 3) == 0) THEN
            S = S + 2 * F(A + I * H)
        ELSE
            S = S + 3 * F(A + I * H)
        END IF
    END DO

    I3 = (3 * H / 8) * (F(A) + S + F(B))

    ! Output results
    OPEN(UNIT = 11, FILE = "out5q6.txt")
    WRITE(11, *) "THE APPROXIMATION IS : ", I3
    CLOSE(11)

END PROGRAM

FUNCTION F(X)
    F = EXP(3 * X) * SIN(2 * X)
END FUNCTION
