PROGRAM QUES
    IMPLICIT NONE
    INTEGER :: I, J, K, N
    REAL :: TOL = 0.001, F, G
    REAL, DIMENSION(:), ALLOCATABLE :: P

    WRITE(*,*) "ENTER MAXIMUM NUMBER OF ITERATIONS: "
    READ(*,*) N

    ALLOCATE(P(0:N))
    P(0) = 0.6
    OPEN(UNIT=10, FILE="out4q2.txt")

    WRITE(10, '(4A10)') "ITER", "Pn-1", "Pn", "F(Pn)"

    DO I = 1, N
        P(I) = G(P(I-1))
        WRITE(10, '(I10, 3F10.5)') I, P(I-1), P(I), F(P(I))

        IF (ABS(P(I) - P(I-1)) < TOL) THEN
            WRITE(10,*) "ROOT IS: ", P(I)
            EXIT
        END IF
    END DO

    CLOSE(10)
END PROGRAM

REAL FUNCTION G(X)
    G = 2**(-X) - X**3 + 0.5 * X**2
    RETURN
END FUNCTION

REAL FUNCTION F(X)
    F = -2**(-X) + X**3 - 0.5 * X**2 + X
    RETURN
END FUNCTION
