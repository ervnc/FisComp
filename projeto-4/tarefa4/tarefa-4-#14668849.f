      program ex4
        implicit real*8 (a-h, o-z)

        open(1, file="graficoOmega4-f0.5.dat")
        open(2, file="graficoOmega4-f1.2.dat")

        al = 9.8d0
        g = 9.8d0
        am = 1.0d0
        pi = acos(-1d0)

        delta_t = 0.04d0

        gamma = 0.5d0
        Omega = 2.0d0/3.0d0

        t_max = 100.0d0

        do i = 1, 2
          if (i .eq. 1) then
            f0 = 0.5d0
          else 
            f0 = 1.2d0
          end if

          omega0 = 0.0d0
          omega0_2 = 0.0d0
          theta0 = 0.2d0
          theta0_2 = theta0 + 0.001d0  
          t = 0.0d0

          do while (t .LT. t_max)
            ! Primeiro pêndulo
            omega1 = omega0 + (-(g/al)*sin(theta0) - gamma*omega0
     &      +f0*sin(Omega*t))*delta_t
        
            theta1 = theta0 + omega1*delta_t

            theta1 = mod(theta1 + pi, 2.0d0 * pi) - pi

            theta0 = theta1
            omega0 = omega1

            !Segundo pêndulo
            omega2 = omega0_2 + (-(g/al)*sin(theta0_2) - gamma*omega0_2
     &      +f0*sin(Omega*t))*delta_t
        
            theta2 = theta0_2 + omega2*delta_t

            theta2 = mod(theta2 + pi, 2.0d0 * pi) - pi

            theta0_2 = theta2
            omega0_2 = omega2

            t = t + delta_t

            if (i .eq. 1) then
              write(1,*) theta1, omega1
            else
              write(2,*) theta1, omega1
            end if 
          end do
        end do

        close(1); close(2)
      end program ex4