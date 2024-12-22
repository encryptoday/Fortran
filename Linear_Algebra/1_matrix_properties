PROGRAM QUES4
    IMPLICIT NONE
    INTEGER :: I, J, C = 1, K
    COMPLEX, DIMENSION(3, 3) :: M, MPRO, CONJ, CONJ_TRANS
    COMPLEX :: S = (0.0, 0.0), P = (1.0, 0.0), TEMP
    REAL, DIMENSION(18) :: A
    REAL, DIMENSION(3, 3) :: MOD, ARG
    LOGICAL :: HERMATIAN

    ! Reading input from file
    OPEN(UNIT = 12, FILE = "in1q4.txt")
    READ(12, *) (A(I), I = 1, 18)
    CLOSE(12)

    ! Initializing matrix M
    DO I = 1, 3
        DO J = 1, 3
            M(I, J) = CMPLX(A(C), A(C + 1))
            C = C + 2
        END DO
    END DO

    ! Calculating modulus and argument
    DO I = 1, 3
        DO J = 1, 3
            MOD(I, J) = ABS(M(I, J))
            ARG(I, J) = ATAN2(AIMAG(M(I, J)), REAL(M(I, J))) * 180.0 / 3.14159265359
        END DO
    END DO

    ! Writing modulus and argument to file
    OPEN(UNIT = 10, FILE = "out1q4_1.txt")
    WRITE(10, "('MODULUS',/,3(3F10.3,/))") ((MOD(I, J), J = 1, 3), I = 1, 3)
    WRITE(10, "('ARGUMENT',/,3(3F10.3,/))") ((ARG(I, J), J = 1, 3), I = 1, 3)
    CLOSE(10)

    ! Calculating product and sum of minor diagonal
    S = (0.0, 0.0)
    P = (1.0, 0.0)
    DO I = 1, 3
        DO J = 1, 3
            IF (I + J == 4) THEN
                S = S + M(I, J)
                P = P * M(I, J)
            END IF
        END DO
    END DO

    OPEN(UNIT = 10, FILE = "out1q4_2.txt")
    WRITE(10, "('PRODUCT AND SUM',/, 'SUM:', F10.3, '+', F10.3, 'i',/, 'PRODUCT:', F10.3, '+', F10.3, 'i')") REAL(S), AIMAG(S), REAL(P), AIMAG(P)
    CLOSE(10)

    ! Calculating M * M*
    CONJ = CONJG(M)
    DO I = 1, 3
        DO J = 1, 3
            TEMP = (0.0, 0.0)
            DO K = 1, 3
                TEMP = TEMP + M(I, K) * CONJ(K, J)
            END DO
            MPRO(I, J) = TEMP
        END DO
    END DO

    OPEN(UNIT = 10, FILE = "out1q4_3.txt")
    WRITE(10, "('PRODUCT OF M AND COMPLEX CONJUGATE OF M:',/, 3(3(F10.5, '+', F10.5, 'i', 5X),/))") ((REAL(MPRO(I, J)), AIMAG(MPRO(I, J)), J = 1, 3), I = 1, 3)
    CLOSE(10)

    ! Checking if the matrix is Hermitian
    DO I = 1, 3
        DO J = 1, 3
            CONJ_TRANS(I, J) = CONJG(M(J, I))
        END DO
    END DO

    HERMATIAN = .TRUE.
    DO I = 1, 3
        DO J = 1, 3
            IF (M(I, J) /= CONJ_TRANS(I, J)) THEN
                HERMATIAN = .FALSE.
                EXIT
            END IF
        END DO
        IF (.NOT. HERMATIAN) EXIT
    END DO

    OPEN(UNIT = 11, FILE = "out1q4_4.txt")
    IF (.NOT. HERMATIAN) THEN
        WRITE(11, *) "NOT HERMATIAN MATRIX"
    ELSE
        WRITE(11, *) "HERMATIAN MATRIX"
    END IF
    CLOSE(11)
END PROGRAM QUES4
