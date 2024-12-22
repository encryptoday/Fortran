PROGRAM QUES7
    IMPLICIT NONE

    INTEGER :: I, J, N
    REAL :: S, F, X, A, B, H

    ! Initialization
    A = EXP(1.0)
    B = 2 * EXP(1.0)
    N = 30
    H = (B - A) / N
    S = 0
    I = 0

    ! Trapezoidal Rule with weighted sums
    DO
        S = S + F(A + I * H) + 5 * F(A + (I + 1) * H) + F(A + (I + 2) * H) + &
            6 * F(A + (I + 3) * H) + F(A + (I + 4) * H) + 5 * F(A + (I + 5) * H) + &
            F(A + (I + 6) * H)
        I = I + 6

        IF (I == N) EXIT
    END DO

    S = (3 * H / 10) * S

    ! Output the result
    OPEN(UNIT = 11, FILE = "out5q7.txt")
    WRITE(11, *) "THE APPROXIMATION IS : ", S
    CLOSE(11)

END PROGRAM

FUNCTION F(X)
    F = 1 / (X * LOG(X))
END FUNCTION
