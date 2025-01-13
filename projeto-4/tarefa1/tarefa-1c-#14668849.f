      program ex1c
        implicit real*8 (a-h, o-z)

        open(1, file="graficoTheta-1c.dat")
        open(2, file="energia-1c.dat")
        
        al = 9.8d0
        g = 9.8d0
        am = 1.0d0
        pi = acos(-1d0)
        
        delta_t = 0.01d0
        omega0 = 0.0d0
        theta0 = 0.5d0

        f0 = 0.2d0
        gamma = 1.0d0
        Omega = 1.0d0

        t_max = 100.0d0
        t = 0.0d0

        do while(t .LT. t_max)
          
          ! Pêndulo sujeito a forças dissipativas e sob ação de forças externas
          ! Gamma: termo da escala resistiva
          ! F0: amplitude da força externa
          ! Omega: frequência da força externa
          omega1 = omega0 + (-(g/al)*sin(theta0) - gamma*omega0
     &    +f0*sin(Omega*t))*delta_t

          theta1 = theta0 + omega1*delta_t

          theta1 = mod(theta1, 2.0d0*pi)

          ! Energia E(t) no sistema com forças dissipativas
          e=0.5d0*am*(al**2)*(omega1**2)+am*g*al*(1.0d0-cos(theta1))
          
          theta0 = theta1
          omega0 = omega1

          write(1, *) t, theta1
          write(2, *) t, e

          t = t + delta_t
        end do
        
        close(1); close(2)

      end program ex1c