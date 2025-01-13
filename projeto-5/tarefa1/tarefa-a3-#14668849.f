      program exa3
        implicit real*8 (a-h, o-z)

        dimension raio_ua(9), vx(9), vy(9), ax(9), ay(9)
        dimension x(9), y(9), x_ant(9), y_ant(9)
        dimension semi_eixo(9), excentricidade(9)
        character*8 planetas(9)

        open(1, file='saida-a3-#14668849.dat')
        open(2, file='saida-a3-#14668849-2.dat')

        pi = acos(-1.0)

        raio_ua = (/0.39, 0.72, 1.0, 1.52, 5.2, 9.58, 
     &  19.22, 30.05, 39.48/)
        excentricidade = (/0.206, 0.007, 0.017, 0.093, 
     &  0.048, 0.056, 0.046, 0.010, 0.248/)

        planetas = (/ "Mercurio", "Venus   ", "Terra   ", "Marte   ", 
     &  "Jupiter ", "Saturno ", "Urano   ", "Netuno  ", "Plutao  " /)

        write(2, *) "Planeta", "Raio (UA)", "Periodo (anos)", "T^2/R^3"

        do i = 1, 9
          semi_eixo(i) = raio_ua(i)/(1.0 - excentricidade(i))
        end do
        
        do i = 1, 9
          x(i) = raio_ua(i)
          y(i) = 0.0

          vx(i) = 0.0
          vy(i) = sqrt((4.0*pi**2*(2.0/raio_ua(i) - 1.0/semi_eixo(i))))
          
          r = sqrt(x(i)**2 + y(i)**2)
          ax(i) = -((4.0*(pi**2)*x(i))/r**3)
          ay(i) = -((4.0*(pi**2)*y(i))/r**3)

          t = 0.0
          delta_t = 1.0E-3
          t_max = semi_eixo(i)**1.5

          x_ant(i) = x(i) - (vx(i)*delta_t) + (0.5*ax(i)*delta_t**2)
          y_ant(i) = y(i) - (vy(i)*delta_t) + (0.5*ay(i)*delta_t**2)

          do while (t .lt. t_max)

            r = sqrt(x(i)**2 + y(i)**2)

            ax(i) = -((4.0*(pi**2)*x(i))/r**3)
            ay(i) = -((4.0*(pi**2)*y(i))/r**3)

            x_new = 2.0*x(i) - x_ant(i) + (ax(i) * delta_t**2)
            y_new = 2.0*y(i) - y_ant(i) + (ay(i) * delta_t**2)

            x_ant(i) = x(i)
            y_ant(i) = y(i)

            x(i) = x_new
            y(i) = y_new

            r = sqrt(x(i)**2 + y(i)**2)
            
            !write(1, *) t, x(i), y(i), r

            t = t + delta_t

          end do

          raio = (t_max**2)/(raio_ua(i)**3)
          write(2, *) planetas(i), raio_ua(i), t_max, raio

        end do
        
        close(1)

      end program exa3