      program exa1
        implicit real*8 (a-h,o-z)

        open(1, file="saida-a1-#14668849.dat")

        xt = 1.0
        yt = 0.0

        pi = acos(-1.0)

        r = sqrt(xt**2 + yt**2)

        vxt = 0.0
        vyt = 2*pi*r

        t = 0.0
        delta_t = 1.0E-4
        t_max = 1

        x_old = xt - vxt*delta_t
        y_old = yt - vyt*delta_t

        do while (t .lt. t_max)

          r = sqrt(xt**2 + yt**2)

          ax = -(4*pi**2*xt)/r**3
          ay = -(4*pi**2*yt)/r**3

          xt2 = 2.0*xt - x_old + (ax*delta_t**2)
          yt2 = 2.0*yt - y_old + (ay*delta_t**2)

          x_old = xt
          y_old = yt
          yt = yt2
          xt = xt2

          t = t + delta_t
          
          write(1, *) xt, yt

        end do

        close(1)
        
      end program exa1