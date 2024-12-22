PROGRAM QUES2
    IMPLICIT NONE
    INTEGER :: N, I, AN
    INTEGER, DIMENSION(:), ALLOCATABLE :: ARR

    ! Read input value of N
    READ(*,*) N
    ALLOCATE(ARR(N))

    ! Initialize the first two terms of the sequence
    ARR(0) = 0
    ARR(1) = 1

    ! Open output file for writing
    OPEN(UNIT=10, FILE="out2q2.txt")
    WRITE(10, '(A10, 5X, A10)') "VALUE OF N", "NTH TERM"

    ! Calculate the sequence
    DO I = 2, N
        IF (MOD(I, 2) == 0) THEN
            ARR(I) = ARR(I / 2)
        ELSE
            ARR(I) = ARR((I - 1) / 2) + ARR((I + 1) / 2)
        END IF
    END DO

    ! Write the sequence to the output file
    DO I = 0, N - 1
        WRITE(10, '(I5, 10X, I5)') I, ARR(I)
    END DO

    ! Close the output file
    CLOSE(10)
END PROGRAM
