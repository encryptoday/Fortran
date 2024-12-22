PROGRAM QUES4
    IMPLICIT NONE
    INTEGER :: I, J, N
    LOGICAL :: NOT_SINGULAR
    REAL, DIMENSION(3,3) :: A, AINV
    
    ! Open files for input and output
    OPEN(10, FILE='in3q4.txt')
    OPEN(11, FILE='out3q4.txt')
    
    ! Read the matrix A from the input file
    READ(10,*) ((A(I,J), J=1, 3), I=1, 3)
    
    ! Write the input matrix A to the output file
    WRITE(11, '("MATRIX A IS : ", /, 3(3F10.4, /), /)')
    WRITE(11, *) ((A(I,J), J=1, 3), I=1, 3)
    
    ! Call the subroutine to compute the inverse of A
    CALL INV(A, AINV, NOT_SINGULAR)
    
    ! Check if the matrix is singular and write results accordingly
    IF (NOT_SINGULAR) THEN
        WRITE(11, '("A INVERSE IS : ", /, 3(3F10.4, /), /)')
        WRITE(11, *) ((AINV(I,J), J=1, 3), I=1, 3)
    ELSE
        WRITE(11, *) "A IS SINGULAR"
    END IF
END PROGRAM QUES4

SUBROUTINE INV(A, AINV, NOT_SINGULAR)
    IMPLICIT NONE
    REAL, DIMENSION(3,3), INTENT(IN) :: A
    REAL, DIMENSION(3,3), INTENT(OUT) :: AINV
    LOGICAL, INTENT(OUT) :: NOT_SINGULAR
    REAL :: DET
    REAL, DIMENSION(3,3) :: COFACTOR
    
    ! Compute the determinant of A
    DET = A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2) &
        - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1) &
        + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1)
    
    ! Check if the determinant is zero
    IF (DET == 0) THEN
        NOT_SINGULAR = .FALSE.
        RETURN
    END IF
    
    ! Compute the cofactor matrix
    COFACTOR(1,1) = +(A(2,2)*A(3,3) - A(2,3)*A(3,2))
    COFACTOR(1,2) = -(A(2,1)*A(3,3) - A(2,3)*A(3,1))
    COFACTOR(1,3) = +(A(2,1)*A(3,2) - A(2,2)*A(3,1))
    COFACTOR(2,1) = -(A(1,2)*A(3,3) - A(1,3)*A(3,2))
    COFACTOR(2,2) = +(A(1,1)*A(3,3) - A(1,3)*A(3,1))
    COFACTOR(2,3) = -(A(1,1)*A(3,2) - A(1,2)*A(3,1))
    COFACTOR(3,1) = +(A(1,2)*A(2,3) - A(1,3)*A(2,2))
    COFACTOR(3,2) = -(A(1,1)*A(2,3) - A(1,3)*A(2,1))
    COFACTOR(3,3) = +(A(1,1)*A(2,2) - A(1,2)*A(2,1))
    
    ! Compute the inverse of A
    AINV = TRANSPOSE(COFACTOR) / DET
    NOT_SINGULAR = .TRUE.
    RETURN
END SUBROUTINE INV
