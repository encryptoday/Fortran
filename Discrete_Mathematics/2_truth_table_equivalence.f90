PROGRAM QUES2
    IMPLICIT NONE
    INTEGER :: I, J, TAG = 1
    LOGICAL :: T, F, P, Q, IMPLIES, BC1, BC2

    ! Initialize logical values
    T = .TRUE.
    F = .FALSE.

    ! Open output file
    OPEN(11, FILE = 'out6q2.txt')

    ! Write header for first truth table
    WRITE(11, '(/, A)') "TRUTH TABLE FOR P->Q AND Q<->P: "
    WRITE(11, 21) "P", "Q", "P->Q", "Q<->P"

21 FORMAT(A1, 2X, A1, 2X, A4, 2X, A5)

    ! Loop through all combinations of P and Q for the first truth table
    DO I = 0, 1
        CALL CONV_LOGICAL(P, I)
        DO J = 0, 1
            CALL CONV_LOGICAL(Q, J)
            CALL BICONDITIONAL(Q, P, BC1)

            ! Write logical values and their results
            WRITE(11, 22) P, Q, IMPLIES(P, Q), BC1
        END DO
    END DO

    ! Format for the first table
22 FORMAT(L, 2X, L, 2X, L2, 4X, L3)

    ! Write header for second truth table
    WRITE(11, '(/, A)') "TRUTH TABLE FOR P<->Q AND (P&Q)||(~P&~Q):"
    WRITE(11, 23) "P", "Q", "P<->Q", "(P&Q)||(~P&~Q)"

23 FORMAT(A1, 2X, A1, 2X, A5, 2X, A14)

    ! Loop through all combinations of P and Q for the second truth table
    DO I = 0, 1
        CALL CONV_LOGICAL(P, I)
        DO J = 0, 1
            CALL CONV_LOGICAL(Q, J)
            CALL BICONDITIONAL(P, Q, BC2)

            ! Write logical values and their results
            WRITE(11, 24) P, Q, BC2, (P .AND. Q) .OR. (.NOT. P .AND. .NOT. Q)

            ! Check equivalence of the logical expressions
            IF (BC2 .NEQV. ((P .AND. Q) .OR. (.NOT. P .AND. .NOT. Q))) THEN
                TAG = 0
            END IF
        END DO
    END DO

    ! Format for the second table
24 FORMAT(L, 2X, L, 2X, L3, 4X, L7)

    ! Output the result
    WRITE(11, '(/, A)', ADVANCE = 'NO') "THE LOGICAL STATEMENTS P<->Q AND (P&Q)||(~P&~Q) ARE:"

    IF (TAG == 0) THEN
        WRITE(11, *) "NOT EQUIVALENT"
    ELSE IF (TAG == 1) THEN
        WRITE(11, *) "EQUIVALENT"
    END IF

    CLOSE(11)

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

FUNCTION IMPLIES(A, B)
    LOGICAL :: A, B, T, F, IMPLIES

    ! Initialize logical values
    T = .TRUE.
    F = .FALSE.

    IMPLIES = .NOT. A .OR. B

END FUNCTION

SUBROUTINE BICONDITIONAL(A, B, C)
    LOGICAL :: A, B, C, T, F, IMPLIES

    ! Initialize logical values
    T = .TRUE.
    F = .FALSE.

    C = IMPLIES(A, B) .AND. IMPLIES(B, A)

END SUBROUTINE
