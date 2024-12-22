PROGRAM QUES1
    IMPLICIT NONE
    INTEGER :: I, J, N
    DOUBLE PRECISION :: P, T = 0.43
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: X, Y, L

    OPEN(10, FILE='in5q1.txt')
    READ(10,*) N
    ALLOCATE(X(0:N), Y(0:N), L(0:N))
    READ(10,*) (X(I), I = 0, N)
    READ(10,*) (Y(I), I = 0, N)
    CLOSE(10)

    OPEN(UNIT=11, FILE="out5q1.txt")
    WRITE(11,*) "X", "F(X)"

    DO I = 0, N
        WRITE(11, '(F10.5, F15.6)') X(I), Y(I)
    END DO

    P = 0
    DO I = 0, N
        L(I) = 1
        DO J = 0, N
            IF (J /= I) THEN
                L(I) = L(I) * ((T - X(J)) / (X(I) - X(J)))
            END IF
        END DO
        P = P + L(I) * Y(I)
    END DO

    WRITE(11, '(/, "P(", F10.5, ") =", F15.6)') T, P
    CLOSE(11)

END PROGRAM
