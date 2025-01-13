      program exb1
        implicit real*8 (a-h,o-z)

        open(1, file="terrab2.dat")
        open(2, file="jupiterb2.dat")

        pi = acos(-1.0)
        am_s = 1.989E30
        am_t = 5.972E24
        am_j = 1.898E27*1000

        ! Posicao Inicial - SOL
        xs = 0.0
        ys = 0.0

        ! Posicao Inicial e velocidades iniciais - TERRA
        xt = 1.0
        yt = 0.0
        vxt = 0.0
        vyt = (2.0*pi)/sqrt(xt)

        ! Posicao Inicial e velocidades iniciais - JUPITER
        xj = 5.2
        yj = 0.0
        vxj = 0.0
        vyj = (2.0*pi)/sqrt(xj)

        t = 0.0
        delta_t = 1.0E-4
        t_max = xj**1.5

        ! Posição em relação aos planetas
        rt = sqrt((xt-xs)**2 + (yt-xs)**2)
        rj = sqrt((xj-xs)**2 + (yj-xs)**2)
        rtj = sqrt((xt-xj)**2 + (yt-yj)**2)

        ! Aceleração da Terra em relação ao Sol e a Jupiter - X
        axt = -(4.0*(pi**2)*xt)/rt**3
     &  - (4.0*(pi**2)*(am_j/am_s)*(xt-xj))/rtj**3
        
        ! Aceleração da Terra em relação ao Sol e a Jupiter - Y
        ayt = -(4.0*(pi**2)*yt)/rt**3
     &  - (4.0*(pi**2)*(am_j/am_s)*(yt-yj))/rtj**3

        ! Posição anterior Terra
        x_ant_t = xt - (vxt*delta_t) + (0.5*axt*delta_t**2)
        y_ant_t = yt - (vyt*delta_t) + (0.5*ayt*delta_t**2)

        ! Aceleração de Jupiter em relação ao Sol e a Terra - X
        axj = -(4.0*(pi**2)*xj)/rj**3
     &  - (4.0*(pi**2)*(am_t/am_s)*(xj-xt))/rtj**3

        ! Aceleração de Jupiter em relação ao Sol e a Terra - Y
        ayj = -(4.0*(pi**2)*yj)/rj**3
     &  - (4.0*(pi**2)*(am_t/am_s)*(yj-yt))/rtj**3

        ! Posição anterior Jupiter
        x_ant_j = xj - (vxj*delta_t) + (0.5*axj*delta_t**2)
        y_ant_j = yj - (vyj*delta_t) + (0.5*ayj*delta_t**2)

        do while (t .le. t_max)

          rt = sqrt((xt-xs)**2 + (yt-xs)**2)
          rj = sqrt((xj-xs)**2 + (yj-xs)**2)
          rtj = sqrt((xt-xj)**2 + (yt-yj)**2)

          axt = -(4.0*(pi**2)*xt)/rt**3
     &    - (4.0*(pi**2)*(am_j/am_s)*(xt-xj))/rtj**3

          ayt = -(4.0*(pi**2)*yt)/rt**3
     &    - (4.0*(pi**2)*(am_j/am_s)*(yt-yj))/rtj**3

          axj = -(4.0*(pi**2)*xj)/rj**3
     &    - (4.0*(pi**2)*(am_t/am_s)*(xj-xt))/rtj**3

          ayj = -(4.0*(pi**2)*yj)/rj**3
     &  - (4.0*(pi**2)*(am_t/am_s)*(yj-yt))/rtj**3

          x_new_t = 2.0*xt - x_ant_t + (axt * delta_t**2)
          y_new_t = 2.0*yt - y_ant_t + (ayt * delta_t**2)

          x_new_j = 2.0*xj - x_ant_j + (axj * delta_t**2)
          y_new_j = 2.0*yj - y_ant_j + (ayj * delta_t**2)

          x_ant_t = xt
          y_ant_t = yt
          y_ant_j = yj
          x_ant_j = xj

          xt = x_new_t
          yt = y_new_t
          xj = x_new_j
          yj = y_new_j

          write(1, *) xt, yt
          write(2, *) xj, yj

          t = t + delta_t
        end do

        close(1); close(2)

      end program exb1