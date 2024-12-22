PROGRAM QUES6
    IMPLICIT NONE
    INTEGER :: I, J, N, FACT, Z
    DOUBLE PRECISION :: P, S, D, H
    DOUBLE PRECISION, DIMENSION(1:2) :: T
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: F
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: X

    T(1) = 15.0
    T(2) = 30.0

    OPEN(10, FILE="in4q6.txt")
    READ(10,*) N
    ALLOCATE(X(0:N), F(0:N, 0:N))
    READ(10,*) (X(I), I = 0, N)
    READ(10,*) (F(I, 0), I = 0, N)
    CLOSE(10)

    OPEN(UNIT=11, FILE="out4q6.txt")
    WRITE(11,*) "NEWTON BACKWARD DIFFERENCE TABLE: "
    WRITE(11, *)

    WRITE(11, '(A, A)', ADVANCE="NO") "X", "Y"
    DO I = 0, N - 1
        WRITE(11, '(A, I2, A)', ADVANCE="NO") "D(", I + 1, ")Y"
    END DO
    WRITE(11, *)

    DO I = 0, N
        WRITE(11, '(F8.3, 4X)', ADVANCE='NO') X(I)
        DO J = 0, I
            F(I + 1, J + 1) = F(I + 1, J) - F(I, J)
            WRITE(11, '(F15.5, 2X)', ADVANCE='NO') F(I, J)
        END DO
        WRITE(11, *)
    END DO

    !------- INTERPOLATING -------
    DO Z = 1, 2
        H = X(1) - X(0)
        P = (T(Z) - X(N)) / H
        S = F(N, 0)
        D = 1
        DO I = 0, N - 1
            D = D * (P + I)
            S = S + F(N, I + 1) * (D / FACT(I + 1))
        END DO
        WRITE(11, '(/,"P(", F8.3, ")", F15.5)') T(Z), S
    END DO

END PROGRAM

RECURSIVE FUNCTION FACT(N) RESULT(ANS)
    INTEGER :: ANS
    IF (N == 0) THEN
        ANS = 1
    ELSE
        ANS = N * FACT(N - 1)
    END IF
END FUNCTION
