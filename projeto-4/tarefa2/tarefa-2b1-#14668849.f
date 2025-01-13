      program ex2b1
        implicit real*8 (a-h, o-z)
        
        al = 9.8d0
        g = 9.8d0
        am = 1.0d0
        pi = acos(-1d0)

        omega0 = 0.0d0
        theta0 = 0.5d0
        delta_t = 0.01d0

        f0 = 0.0d0
        gamma = 0.0d0
        Omega = 0.0d0

        t_max = 100.0d0
        t = 0.0d0
        icruzou = 0
        T_simulation = 0.0d0
        t_cross = 0.0d0

        ! Método de Euler-Cromer para achar o período
        do while(t .LT. t_max)
          
          omega1 = omega0 + (-(g/al)*sin(theta0) - gamma*omega0
     &    +f0*sin(Omega*t))*delta_t

          theta1 = theta0 + omega1*delta_t

          theta1 = mod(theta1, 2.0d0*pi)

          if (theta1*theta0 .LT. 0.0d0) then
            icruzou = icruzou + 1
            if (icruzou .EQ. 1) then
              t_cross = t
            elseif (icruzou .EQ. 3) then
              T_simulation = t - t_cross
              exit
            end if
          end if

          theta0 = theta1
          omega0 = omega1

          t = t + delta_t
        end do

        ! Método da integral elíptica para achar o período
        n = 10000
        delta_theta = theta0/n
        T_integral = 0.0d0

        do i = 1, n
          theta1 = (i - 0.5d0)*delta_theta ! Ponto médio
          aintegrando = 1.0d0/sqrt(cos(theta1)-cos(theta0))
          T_integral = T_integral + aintegrando*delta_theta
        end do
        
        T_integral = T_integral*sqrt(2.0d0*al/g)
        
        write(*, *) 'Período sem integral: ', T_simulation
        write(*, *) 'Período com integral: ', T_integral*2.0d0

        close(1)
      end program ex2b1