      program ex2b3
        implicit real*8 (a-h, o-z)

        open(1, file="graficoTheta2b3.dat")

        al = 9.8d0
        g = 9.8d0
        am = 1.0d0
        pi = acos(-1d0)

        omega0 = 0.0d0
        theta0 = 0.5d0
        delta_t = 0.001d0

        f0 = 0.0d0
        gamma = 0.5d0
        Omega = 0.0d0

        t_max = 100.0d0
        t = 0.0d0
        
        ! Amortecimento subcrítico
        do while(t .LT. t_max)
          omega1 = omega0 + (-(g/al)*sin(theta0) - gamma*omega0
     &    +f0*sin(Omega*t))*delta_t
     
          theta1 = theta0 + omega1*delta_t

          theta1 = mod(theta1, 2.0d0*pi)

          theta0 = theta1
          omega0 = omega1

          t = t + delta_t

          write(1,*) t, theta1
        end do

        close(1)
      end program ex2b3