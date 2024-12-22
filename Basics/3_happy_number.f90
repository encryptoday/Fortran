PROGRAM QUES7
    IMPLICIT NONE
    LOGICAL :: HAPPY = .FALSE.
    INTEGER :: TEMP, N, A, B, COUNTA = 1, I, SUMA = 0
    INTEGER, DIMENSION(:), ALLOCATABLE :: ARR

    READ(*,*) N
    OPEN(UNIT=10, FILE="out1q7.txt")
    WRITE(10, '("VALUE OF N IS ", I10, /)') N

    TEMP = N
    DO I = 1, 100
        DO
            COUNTA = 0
            TEMP = N
            DO
                COUNTA = COUNTA + 1
                TEMP = TEMP / 10
                IF (TEMP == 0) EXIT
            END DO

            ALLOCATE(ARR(COUNTA))

            TEMP = N
            SUMA = 0
            DO A = 1, COUNTA
                ARR(A) = MOD(TEMP, 10)
                SUMA = SUMA + ARR(A)**2
                TEMP = TEMP / 10
            END DO

            DEALLOCATE(ARR)

            IF (SUMA == 1) THEN
                HAPPY = .TRUE.
                EXIT
            END IF

            IF (SUMA == N) EXIT
            N = SUMA
        END DO
        EXIT
    END DO

    IF (HAPPY .EQV. .TRUE.) THEN
        WRITE(10, *) "HAPPY NUMBER"
    ELSE
        WRITE(10, *) "NOT HAPPY NUMBER"
    END IF

    CLOSE(10)
END PROGRAM
