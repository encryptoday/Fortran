PROGRAM QUES1
    IMPLICIT NONE
    INTEGER :: I, J, K, TAG = 1
    LOGICAL :: T, F, P, Q, R

    ! Initialize logical values
    T = .TRUE.
    F = .FALSE.

    ! Open output file
    OPEN(10, FILE = 'out6q1.txt')

    ! Write header
    WRITE(10, 101) "P", "Q", "R", "P||(Q&R)", "(P&Q)||(P&R)"

101 FORMAT(A1, 2X, A1, 2X, A1, 2X, A8, 2X, A12)

    ! Loop through all combinations of P, Q, and R
    DO I = 0, 1
        CALL CONV_LOGICAL(P, I)
        DO J = 0, 1
            CALL CONV_LOGICAL(Q, J)
            DO K = 0, 1
                CALL CONV_LOGICAL(R, K)

                ! Write logical values and their results
                WRITE(10, 102) P, Q, R, P .OR. (Q .AND. R), (P .OR. Q) .AND. (P .OR. R)

                ! Check equivalence of the logical expressions
                IF ((P .OR. (Q .AND. R)) .NEQV. ((P .OR. Q) .AND. (P .OR. R))) THEN
                    TAG = 0
                END IF
            END DO
        END DO
    END DO

102 FORMAT(L, 2X, L, 2X, L, 2X, L4, 6X, L6)

    ! Output the result
    WRITE(10, '(/, A)', ADVANCE = 'NO') "P||(Q&R) AND (P&Q)||(P&R) ARE :"

    IF (TAG == 0) THEN
        WRITE(10, *) "NOT EQUIVALENT"
    ELSE IF (TAG == 1) THEN
        WRITE(10, *) "EQUIVALENT"
    END IF

    CLOSE(10)

END PROGRAM

SUBROUTINE CONV_LOGICAL(B, K)
    INTEGER :: K
    LOGICAL :: B, T, F

    ! Initialize logical values
    T = .TRUE.
    F = .FALSE.

    IF (K == 0) THEN
        B = F
    ELSE IF (K == 1) THEN
        B = T
    END IF

END SUBROUTINE
