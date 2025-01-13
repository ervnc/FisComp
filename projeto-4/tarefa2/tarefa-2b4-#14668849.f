      program ex2b4
        implicit real*8 (a-h, o-z)

        open(1, file="graficoTheta2b4-f0.dat")
        open(2, file="graficoTheta2b4-f0.5.dat")
        open(3, file="graficoTheta2b4-f1.2.dat")

        open(4, file="graficoOmega2b4-f0.dat")
        open(5, file="graficoOmega2b4-f0.5.dat")
        open(6, file="graficoOmega2b4-f1.2.dat")
        
        al = 9.8d0
        g = 9.8d0
        am = 1.0d0
        pi = acos(-1d0)

        delta_t = 0.04d0

        gamma = 0.5d0
        Omega = 2.0d0/3.0d0

        t_max = 60.0d0
        
        do i = 1, 3

          if (i .eq. 1) then
            f0 = 0.0d0
          else if (i .eq. 2) then
            f0 = 0.5d0
          else
            f0 = 1.2d0
          end if

          omega0 = 0.0d0
          theta0 = 0.2d0
          t = 0.0d0

          do while(t .LT. t_max)
            omega1 = omega0 + (-(g/al)*sin(theta0) - gamma*omega0
     &      +f0*sin(Omega*t))*delta_t
  
            theta1 = theta0 + omega1*delta_t

            theta1 = mod(theta1, 2.0d0*pi)
  
            theta0 = theta1
            omega0 = omega1
  
            t = t + delta_t
            
            if (i .eq. 1) then
              write(1,*) t, theta1
              write(4,*) t, omega1
            else if (i .eq. 2) then
              write(2,*) t, theta1
              write(5,*) t, omega1
            else
              write(3,*) t, theta1
              write(6,*) t, omega1
            end if
          end do
        end do

        close(1); close(2); close(3)
        close(4); close(5); close(6)
      end program ex2b4