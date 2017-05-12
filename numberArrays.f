      PROGRAM numberArrays
c     bigArraySize is max array size. arraySize is an array size user
c     chooses with max being bigArraySize.
      INTEGER     bigArraySize, arraySize, i
      PARAMETER   (bigArraySize = 100)
      REAL        a(bigArraySize)
      DO i = 1, bigArraySize
        a(i) = i
      ENDDO
      WRITE(*,*)'Enter array size:'
      READ(*,*) arraySize
c     validation that arraySize is smaller than bigArraySize
      IF (arraySize .GT. bigArraySize) STOP 'Too large.'
      CALL sub(a, bigArraySize, arraySize)
      WRITE(*,*) 'Enter anything to end program.'
      READ(*,*)
      END

c     subroutine to fill the array with user generated numbers
      SUBROUTINE sub(a, bigArraySize, arraySize)
      INTEGER    bigArraySize, arraySize, i, num
      REAL       a(arraySize)
      DO i = 1, arraySize
        WRITE(*,*) 'Enter number to put in array: '
        READ (*,*) num
        a(i) =  num
      ENDDO
      CALL bubble_sort(a, arraySize)
      RETURN
      END

      SUBROUTINE bubble_sort(a,arraySize)
      INTEGER arraySize,d
      REAL a(arraySize)
      INTEGER i
c     how many times we have passed through the array
      INTEGER pass
c     1 IF sorted; 0 IF not sorted
      INTEGER sorted
      REAL temp
      pass = 1
 1    CONTINUE
      sorted = 1
      DO 2 i = 1,arraySize-pass
        IF(a(i) .GT. a(i+1)) THEN
          temp = a(i)
          a(i) = a(i+1)
          a(i+1) = temp
          sorted = 0
        ENDIF
 2    CONTINUE
      pass = pass +1
      IF(sorted .EQ. 0) GOTO 1
      DO d = 1, arraySize
        WRITE (*,*) d, a(d)
      ENDDO
      RETURN
      END
