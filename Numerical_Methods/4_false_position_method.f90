PROGRAM QUES4
    IMPLICIT NONE
    INTEGER :: I, N, FOUND = 0
    REAL :: A = 0, B = 1, C, TOL = 0.000001, F
    REAL, ALLOCATABLE, DIMENSION(:) :: P

    WRITE(*,*) "INPUT MAXIMUM NUMBER OF ITERATIONS"
    READ(*,*) N

    ALLOCATE(P(0:N))
    P(0) = 0.5

    OPEN(10, FILE='out4q4.txt')
    WRITE(10, '(A13, 2X, A6, 8X, A6, 8X, A6, 8X, A16)') "ITERATION NO:", "A", "B", "PN", "|(PN-PN-1)/PN|"

    DO I = 1, N
        P(I) = (A * F(B) - B * F(A)) / (F(B) - F(A))
        C = P(I)
        WRITE(10, '(I6, 8X, F12.9, 2X, F12.9, 2X, F12.9, 2X, F15.9)') I, A, B, C, ABS(P(I) - P(I-1)) / ABS(P(I))

        IF (F(C) == 0) EXIT

        IF (ABS((P(I) - P(I-1)) / P(I)) < TOL) THEN
            FOUND = 1
            WRITE(10,*) "ROOT IS:", C
            EXIT
        END IF

        IF (F(A) * F(C) < 0) THEN
            B = C
        ELSE
            A = C
        END IF
    END DO

    IF (FOUND == 0) THEN
        WRITE(10,*) "MAXIMUM NUMBER OF ITERATIONS HAVE REACHED, NO ROOT FOUND"
    END IF

    CLOSE(10)
END PROGRAM

REAL FUNCTION F(X)
    F = 230 * X**4 + 18 * X**3 + 9 * X**2 - 221 * X - 9
    RETURN
END FUNCTION
