PROGRAM QUES5
    IMPLICIT NONE
    INTEGER :: J, N, Z
    REAL :: F, G, I1, I2, H, A, B, S, X
    
    A = 0.0
    B = 7.5
    N = 30
    Z = 18

    ! Open output file
    OPEN(UNIT=11, FILE="out5q5.txt")

    ! Trapezoidal rule
    H = (B - A) / N
    S = 0.0

    DO J = 1, N-1
        S = S + F(A + J * H)
    END DO

    I1 = (H / 2) * (F(A) + 2 * S + F(B))

    ! SIMPSON rule
    H = (B - A) / Z
    S = 0.0

    DO J = 1, Z-1
        IF (MOD(J, 2) == 0) THEN
            S = S + 2 * F(A + J * H)
        ELSE
            S = S + 4 * F(A + J * H)
        END IF
    END DO

    I2 = (H / 3) * (F(A) + S + F(B))

    ! Output results
    WRITE(11, '(A14, A15, 3X, A10)') "METHOD", "APPROXIMATE", "ERROR"
    WRITE(11, '(A, F15.5, 3X, F10.5)') "TRAPEZOIDAL", I1, ABS(G(B) - G(A) - I1)
    WRITE(11, '(A, F15.5, 3X, F10.5)') "SIMPSON'S 1/3", I2, ABS(G(B) - G(A) - I2)

    CLOSE(11)

END PROGRAM

FUNCTION F(X)
    F = 1.5 * X**3 - 7 * X - 1 - EXP(X)
END FUNCTION

FUNCTION G(X)
    G = 0.375 * X**4 - 3.5 * X**2 - X - EXP(X)
END FUNCTION
