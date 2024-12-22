PROGRAM ques2
  IMPLICIT NONE
  CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE :: NAME
  CHARACTER(LEN=1), DIMENSION(10) :: VOWEL = (/"A", "E", "I", "O", "U", "a", "e", "i", "o", "u"/)
  INTEGER :: I = 1, N = 0, J, M = 1
  CHARACTER(LEN=1) :: X
  LOGICAL :: FOUND
  INTEGER :: IO

  ! OPENING FILE
  OPEN(UNIT=18, FILE="in1q2.txt", ACTION="READ")
  
  DO
    READ(18, "(A1)", ADVANCE="NO", IOSTAT=IO) X
    IF (IO /= 0) EXIT
    N = N + 1
  END DO

  ALLOCATE(NAME(N), STAT=IO)
  IF (IO /= 0) THEN
    PRINT *, "FAILED ALLOCATION"
    STOP
  END IF

  REWIND(18)

  DO I = 1, N
    READ(18, "(A1)", ADVANCE="NO") NAME(I)
  END DO
  CLOSE(18)

  ! OPENING OUTPUT FILE
  OPEN(UNIT=20, FILE="out1q2.txt")

  !--- WITHOUT SPACE ----!
  DO I = 1, N
    IF (NAME(I) == " ") CYCLE
    WRITE(20, "(A1)", ADVANCE="NO") NAME(I)
  END DO

  WRITE(20, *)

  ! WITHOUT VOWEL ----!
  DO I = 1, N
    FOUND = .FALSE.
    DO J = 1, 10
      IF (VOWEL(J) == NAME(I)) THEN
        FOUND = .TRUE.
        EXIT
      END IF
    END DO
    IF (.NOT. FOUND) WRITE(20, "(A1)", ADVANCE="NO") NAME(I)
  END DO

  WRITE(20, *)

  ! TO UPPERCASE -!
  DO I = 1, N
    IF (NAME(I) >= "a" .AND. NAME(I) <= "z") THEN
      WRITE(20, "(A1)", ADVANCE="NO") CHAR(IACHAR(NAME(I)) - 32)
    ELSE
      WRITE(20, "(A1)", ADVANCE="NO") NAME(I)
    END IF
  END DO

  ! REVERSE ORDER -----!
  WRITE(20, *)
  DO I = N, 1, -1
    WRITE(20, "(A1)", ADVANCE="NO") NAME(I)
  END DO

  CLOSE(20)

END PROGRAM
