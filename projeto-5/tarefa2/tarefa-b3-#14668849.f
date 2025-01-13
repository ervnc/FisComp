      program exb3
        implicit real*8 (a-h, o-z)

        dimension aster_raio(3), aster_vel(3), r_ast_s(3), r_ast_j(3)
        dimension x(3), y(3), ax(3), ay(3), x_ant(3), y_ant(3)

        open(1, file="asteroides1.dat")
        open(2, file="asteroides2.dat")
        open(3, file="asteroides3.dat")

        am_j = 1.898E27
        am_s = 1.989E30
        pi = acos(-1.0)

        xs = 0.0
        ys = 0.0

        xj = 5.2
        yj = 0.0
        vxj = 0.0
        vyj = 2.76

        aster_raio = (/ 3.0, 3.28, 3.7 /)
        aster_vel = (/ 3.63, 3.47, 3.27 /)

        xj_old = xj - vxj * delta_t
        yj_old = yj - vyj * delta_t

        t = 0.0
        delta_t = 1.0E-3
        t_max = 100.0

        do i = 1, 3
          x(i) = aster_raio(i)
          y(i) = 0.0

          r_ast_s(i) = sqrt((x(i))**2 + (y(i))**2)
          r_ast_j(i) = sqrt((x(i)-xj)**2 + (y(i)- yj)**2)
          
          ax(i) = -(4.0*(pi**2)*(x(i)))/r_ast_s(i)**3
     &    - (4.0*(pi**2)*(am_j/am_s)*(x(i)-xj))/r_ast_j(i)**3

          ay(i) = -(4.0*(pi**2)*(y(i)))/r_ast_s(i)**3
     &    - (4.0*(pi**2)*(am_j/am_s)*(y(i)-yj))/r_ast_j(i)**3

          x_ant(i) = x(i)+(0.5*ax(i)*delta_t**2)
          y_ant(i) = y(i)-(aster_vel(i)*delta_t)+(0.5*ay(i)*delta_t**2)
        end do

        do while (t .le. t_max)

          r = sqrt(xj**2 + yj**2)
          ajx = -((4.0*(pi**2)*xj)/r**3)
          ajy = -((4.0*(pi**2)*yj)/r**3)


          xj_new = 2.0*xj - xj_old + (ajx * delta_t**2)
          yj_new = 2.0*yj - yj_old + (ajy * delta_t**2)

          xj_old = xj
          yj_old = yj
          xj = xj_new
          yj = yj_new

          do i = 1, 3
            r_ast_s(i) = sqrt((x(i)-xs)**2 + (y(i)-ys)**2)
            r_ast_j(i) = sqrt((x(i)-xj)**2 + (y(i)-yj)**2)
            
            ax(i) = -(4.0*(pi**2)*(x(i)-xs))/r_ast_s(i)**3
     &      - (4.0*(pi**2)*(am_j/am_s)*(x(i)-xj))/r_ast_j(i)**3
        
            ay(i) = -(4.0*(pi**2)*(y(i)-ys))/r_ast_s(i)**3
     &      - (4.0*(pi**2)*(am_j/am_s)*(y(i)-yj))/r_ast_j(i)**3
  
            x_new = 2.0*x(i) - x_ant(i) + (ax(i)*delta_t**2)
            y_new = 2.0*y(i) - y_ant(i) + (ay(i)*delta_t**2)
  
            x_ant(i) = x(i)
            y_ant(i) = y(i)
            x(i) = x_new
            y(i) = y_new
        
            if (i == 1) then
              write(1, *) x(i), y(i)
            else if (i == 2) then
              write(2, *) x(i), y(i)
            else
              write(3, *) x(i), y(i)
            end if
  
          end do
          t = t + delta_t
        end do         
        
        close(1); close(2); close(3)
      end program exb3