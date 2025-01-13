      program ex5
        implicit real*8 (a-h, o-z)

        open(1, file="graficoOmega5f0.5.dat")
        open(2, file="graficoOmega5f1.2.dat")

        al = 9.8d0
        g = 9.8d0
        am = 1.0d0
        pi = acos(-1d0)

        delta_t = 0.04d0

        gamma = 0.5d0
        Omega = 2.0d0/3.0d0

        t_max = 100000.0d0

        do i = 1, 2
          if (i .EQ. 1) then
            f0 = 0.5d0
          else 
            f0 = 1.2d0
          end if

          t = 0.0d0
          omega0 = 0.0d0
          theta0 = 0.2d0

          do while (t .LT. t_max)  
            
            !Primeiro pêndulo
            omega1 = omega0 + (-(g/al)*sin(theta0) - gamma*omega0
     &      +f0*sin(Omega*t))*delta_t

            theta1 = theta0 + omega1*delta_t

            theta1 = mod(theta1 + pi, 2.0d0*pi) - pi

            omega0 = omega1
            theta0 = theta1

            t = t + delta_t

            if (abs(t-nint(Omega*t/pi)*pi/Omega).LE.delta_t/2.0d0) then
              if (i .eq. 1) then
                write(1,*) theta1, omega1
              else
                write(2,*) theta1, omega1
              end if
            end if
          end do
        end do

        close(1); close(2)
      end program ex5