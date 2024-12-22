PROGRAM QUES7
    IMPLICIT NONE
    INTEGER :: N, F

    ! Read the number of vertices
    WRITE(*,*) " NUMBER OF VERTICES N :"
    READ(*,*) N

    ! Open output file and write results
    OPEN(UNIT = 11, FILE = "out6q7.txt")
    WRITE(11,*) "NUMBER OF VERTICES : ", N
    WRITE(11,*) "NUMBER OF CIRCUITS : ", F(N - 1) / 2
    CLOSE(11)

END PROGRAM

RECURSIVE FUNCTION F(N) RESULT(ANS)
    IMPLICIT NONE
    INTEGER :: N, ANS

    ! Base case: if N is 0
    IF (N == 0) THEN
        ANS = 1
    ELSE
        ANS = N * F(N - 1)
    END IF

    RETURN
END FUNCTION
