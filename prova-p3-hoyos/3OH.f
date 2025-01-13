      program ex1
        implicit real(a-h,o-z)

        open(1, file='30H.dat')

        ak = 5.5
        pi = acos(-1.0e0)
        t = 0.0
        t_max = 2*pi
        delta_t = 1.0E-3

        r1x = 9.0
        r1y = 0.0
        r2x = 1.1
        r2y = 0.0
        v1x = 0.0
        v1y = 1.0
        v2x = 0.0
        v2y = 1.0

        dx = r1x - r2x
        dy = r1y - r2y
        
        d = sqrt(dx**2 + dy**2)

        write(1, *) t, d

        do while (t .le. t_max)

          a1x = r1x - ak*(r1x - r2x)
          a2x = r2x - ak*(r2x - r1x)

          a1y = r1y - ak*(r1y - r2y)
          a2y = r2y - ak*(r2y - r1y)

          v1x = v1x + a1x*delta_t
          v2x = v2x + a2x*delta_t

          v1y = v1y + a1y*delta_t
          v2y = v2y + a2y*delta_t

          r1x = r1x + v1x*delta_t
          r2x = r2x + v2x*delta_t

          r1y = r1y + v1y*delta_t
          r2y = r2y + v2y*delta_t

          d = sqrt((r1x - r2x)**2 + (r1y - r2y)**2)

          t = t + delta_t
          write(1, *) t, d
      end do

      close(1)

      end program ex1