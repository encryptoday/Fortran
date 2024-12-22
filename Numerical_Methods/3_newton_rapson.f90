PROGRAM QUES3
    IMPLICIT NONE
    INTEGER :: I, J, N, FOUND
    REAL :: X, TOL = 0.00001, F, DF
    REAL, ALLOCATABLE, DIMENSION(:) :: P

    OPEN(10, FILE='out4q3.txt')
    WRITE(*,*) "ENTER MAXIMUM NUMBER OF ITERATIONS: "
    READ(*,*) N

    ALLOCATE(P(0:N))

101 FORMAT(A, 3X, A10, 5X, A15, 5X, A15, 7X, A15, /)

    DO I = 1, 3
        FOUND = 0

        IF (I == 1) THEN
            P(0) = -4
            WRITE(10,*) "FOR X0 = -4"
        ELSE IF (I == 2) THEN
            P(0) = 1
            WRITE(10,*) "FOR X0 = 1"
        ELSE IF (I == 3) THEN
            P(0) = -2
            WRITE(10,*) "FOR X0 = -2"
        END IF

        WRITE(10, 101) "ITERATION NO", "PN-1", "F'(PN-1)", "PN", "F(PN)"

        DO J = 1, N
            P(J) = P(J-1) - (F(P(J-1)) / DF(P(J-1)))
            WRITE(10, 102) J, P(J-1), DF(P(J-1)), P(J), F(P(J))

            IF (ABS(P(J) - P(J-1)) < TOL) THEN
                WRITE(10, '(/, A, F20.7, /)') "ROOT IS: ", P(J)
                FOUND = 1
                EXIT
            END IF
        END DO

        IF (FOUND == 0) WRITE(10,*) "MAX ITERATIONS REACHED, NO ROOT FOUND"
    END DO

102 FORMAT(I4, 2X, F20.7, 2X, F20.7, 2X, F20.7, 2X, F20.7)

END PROGRAM

REAL FUNCTION F(X)
    F = 16*X**4 + 88*X**3 + 159*X**2 + 76*X - 240
END FUNCTION

REAL FUNCTION DF(X)
    DF = 64*X**3 + 264*X**2 + 318*X + 76
END FUNCTION
