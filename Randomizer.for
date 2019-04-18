      program main
      implicit none
      real kfg, dkfg
      integer N, mems(268435456), jN, generateProfile
      read *, N, kfg, dkfg
      call generateDiag(mems(1), N, dkfg)
      call srand(time())
      call printRarray(mems(1:N), N, "diag.txt", 8, .TRUE.)
      jN = generateProfile(mems(1), mems(40000), mems(50000), N, kfg)
      call printIarray(mems(1:N + 1), N + 1, "indexes.txt", 11, .TRUE.)
      print *, mems(1:N + 1)
      call printIarray(mems(40000), jN, "ja.txt", 6, .TRUE.)
      print *, mems(40000: 40000 + jN)
      call printRarray(mems(50000), jN, "elems.txt", 9, .TRUE.)
      mems(500) = N
      mems(501) = jN
      call printIarray(mems(500), 2, "sizes.txt", 9, .FALSE.)
      call generateDiag(mems(1), N, 100.0)
      call printRarray(mems(1:N), N, "vector.txt", 10, .TRUE.)
      print *, time()
      end program main


      integer function generateProfile(id, ja, el, N, kfg)
      implicit none
      integer i, thg, N, l, k, kf
      real kfg, el((N*N - N) / 2)
      integer id(N + 1), ja((N*N - N) / 2)
      kf = 128 - INT(128 * kfg / 100.0)
      id(1) = 1
      k = 1
      do i = 2, N
          id(i) = k
          do l = 1, i - 1
            thg = mod(INT(rand()*100000000), 128)
            if (thg > kf) then
                ja(k) = l
                el(k) = REAL(thg)
                k = k + 1
            end if
          end do
      end do
      id(N + 1) = k
      generateProfile = k - 1
      end function generateProfile

      subroutine printIarray(di, N, fn, fnsz, b)
      implicit none
      logical b
      integer N, fnsz, i
      character (fnsz) fn
      integer di(N)
      if (b) then
        open (57, file=fn,ACTION='WRITE', access='DIRECT',RECL=4)
        do i = 1, N
            write (57, rec=i) di(i)
        end do
      else
        open (57, file=fn, ACTION='WRITE')
        write (57, *) di
      end if
      close(57)
      end subroutine printIarray

      subroutine printRarray(di, N, fn, fnsz, b)
      implicit none
      logical b
      integer N, fnsz, i
      character (fnsz) fn
      real di(N)
       if (b) then
        open (57, file=fn,ACTION='WRITE', access='DIRECT',RECL=4)
        do i = 1, N
            write (57, rec=i) di(i)
        end do
      else
        open (57, file=fn, ACTION='WRITE')
        write (57, *) di
      end if
      close(57)
      end subroutine printRarray

      subroutine generateDiag(di, N, kfg)
      implicit none
      real di(N), kfg
      integer N, i, thg, kf
      kf = 128 - INT(128 * kfg / 100.0)
      do i = 1, N
        di(i) = 0.0
        thg = mod(INT(rand()*100000000), 128)
        if (thg > kf) then
            di(i) = REAL(thg)
        end if
      end do
      end subroutine generateDiag
