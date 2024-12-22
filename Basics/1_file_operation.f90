PROGRAM a1q1
  IMPLICIT NONE
  CHARACTER, DIMENSION(:), ALLOCATABLE :: CHAR_ARR
  INTEGER, DIMENSION(:), ALLOCATABLE :: INT_ARR
  REAL, DIMENSION(:), ALLOCATABLE :: REAL_ARR1, REAL_ARR2
  INTEGER :: N, IO, I, STAT

  !---------- OPENING FILE 1 --------!
  OPEN(UNIT=10, FILE="InputFile_1.txt", IOSTAT=IO)
  IF (IO /= 0) STOP "Error opening InputFile_1.txt"
  
  READ(10, *) N
  ALLOCATE(CHAR_ARR(N), STAT=STAT)
  IF (STAT /= 0) THEN
    PRINT *, "FAILED TO ALLOCATE CHAR_ARR"
    CLOSE(10)
    STOP
  END IF

  REWIND(10)
  READ(10, 12) (CHAR_ARR(I), I=1,N)
  CLOSE(10)

12 FORMAT(2X, 6(A1, 1X))

  !---------- OPENING FILE 2 --------!
  OPEN(UNIT=20, FILE="InputFile_2.txt", STATUS="OLD", ACTION="READ")
  READ(20, *) N
  ALLOCATE(INT_ARR(N))
  READ(20, *) (INT_ARR(I), I=1,N)
  CLOSE(20)

  !---------- OPENING FILE 3 --------!
  OPEN(UNIT=30, FILE="InputFile_3.txt")
  READ(30, *) N
  ALLOCATE(REAL_ARR1(N))
  READ(30, '(F4.2)') (REAL_ARR1(I), I=1,N)
  CLOSE(30)

  !---------- OPENING FILE 4 --------!
  OPEN(UNIT=40, FILE="InputFile_4.txt")
  READ(40, *) N
  ALLOCATE(REAL_ARR2(N))
  READ(40, *) (REAL_ARR2(I), I=1,N)
  CLOSE(40)

  !--------- OPENING OUTPUT FILE --------!
  OPEN(UNIT=50, FILE="out1q1.txt", ACTION="WRITE")
  WRITE(50, '("FILENAME: InputFile_1.txt",/, I1, 6(1X, A1), /)') N, (CHAR_ARR(I), I=1,N)
  WRITE(50, '("FILENAME: InputFile_2.txt",/, 11, /, 6(12, 1X), /)') N, (INT_ARR(I), I=1,N)
  WRITE(50, '("FILENAME: InputFile_3.txt",/, I1, /, F4.1, /, 2(F3.1, /), F4.2, /, 2(F3.1, /), /)') N, (REAL_ARR1(I), I=1,N)
  WRITE(50, '("FILENAME: InputFile_4.txt",/, I1, /, F4.1, 1X, 2(F3.1, 1X), /, F4.2, 1X, F3.1, /, F3.1)') N, (REAL_ARR2(I), I=1,N)
  CLOSE(50)

END PROGRAM a1q1
