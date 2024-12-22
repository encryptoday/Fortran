PROGRAM NEW
    IMPLICIT NONE
    INTEGER :: I, J, K, N
    REAL, DIMENSION(:), ALLOCATABLE :: P
    REAL :: A = -2.0, B = 0, C, TOL = 0.000001, F
    LOGICAL :: FOUND

    WRITE(*,*) "ENTER MAXIMUM NUMBER OF ITERATIONS: "
    READ(*,*) N

    ALLOCATE(P(0:N))
    OPEN(UNIT=10, FILE="out4q1.txt")

    P(0) = 0
    WRITE(10, 100) "ITERATION NO. ", "A", "B", "Pn", "|Pn-Pn-1|/|Pn|"
100 FORMAT(5A20)

    DO I = 1, N
        C = (A + B) / 2
        P(I) = C
        WRITE(10, 101) I, A, B, P(I), ABS(P(I) - P(I-1)) / ABS(P(I))
101 FORMAT(I20, 3F20.6, F20.6)

        IF (F(C) == 0) THEN
            WRITE(10,*) "ROOT IS: ", C
            EXIT
        ELSE IF (ABS(P(I) - P(I-1)) / ABS(P(I)) < TOL) THEN
            WRITE(10,*) "ROOT IS: ", C
            EXIT
        ELSE
            IF (F(A) * F(C) > 0) THEN
                A = C
            ELSE
                B = C
            END IF
        END IF
    END DO

    CLOSE(10)
END PROGRAM

REAL FUNCTION F(X)
    F = 1.5 * X**3 - 7 * X - 1 - EXP(X)
    RETURN
END FUNCTION
