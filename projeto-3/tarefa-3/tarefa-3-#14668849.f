      program ex3
        real*8 atolerancia, ax, ah, abis(10), anew(10), asec(10)
        real*8 f

        atolerancia = 1.0d-6
        ah = 0.1d0 
        n = int((10.0d0 - (-10.0d0)) / ah)
        k = 1

        do i = 0, n
          ax = -10.0d0 + i*ah
          if (f(ax) * f(ax + ah) .LT. 0.0d0) then
            call bissecao(ax, ax + ah, atolerancia, abis)
            call newton(ax, atolerancia, anew)
            call secante(ax, ax + ah, atolerancia, asec)
      
            write(1, 90) "Raiz", k
            write(1, 110) "Bissecao", "Newton", "Secante", "Exato"

            k = k + 1
      
            do j = 2, 10
              write(1, 120) abis(j), anew(j), asec(j), ax
            end do

            write(1, 130)
          end if
        end do

90      format(a5, i3)
110     format(a15, a15, a15, a15)
120     format(f15.10, f15.10, f15.10, f15.10)
130     format(30("-"))
      end program ex3


      subroutine bissecao(axmenos, axmais, atolerancia, abis)
        real*8 axmenos, axmais, atolerancia, axmedio, f
        real*8 abis(10)

        iter = 0
        imaxiter = 10

        do while (iter .LT. imaxiter)
          axmedio = (axmenos + axmais) / 2.0d0
          abis(iter + 1) = axmedio

          if (abs(axmais - axmenos) / 2.0d0 .LE. atolerancia) exit

          if (f(axmenos) * f(axmedio) .LT. 0.0d0) then
            axmais = axmedio
          else
            axmenos = axmedio
          end if

          iter = iter + 1
        end do

        do while (iter .LT. 10)
          abis(iter+1) = axmedio
          iter = iter + 1
        end do
      end subroutine bissecao

      subroutine newton(ax, atolerancia, anew)
        real*8 ax, atolerancia, axnovo, f, df
        real*8 anew(10)

        iter = 0
        imaxiter = 10

        do while (iter .LT. imaxiter)
          axnovo = ax - f(ax) / df(ax)
          anew(iter + 1) = axnovo

          if (abs(f(axnovo)) .LE. atolerancia) exit
          if (abs(axnovo - ax) .LE. atolerancia) exit

          ax = axnovo
          iter = iter + 1
        end do

        do while (iter .LT. 10)
          anew(iter+1) = axnovo
          iter = iter + 1
        end do
      end subroutine newton

      subroutine secante(axme, axma, atolerancia, asec)
        real*8 axme, axma, atolerancia, axnovo, f
        real*8 asec(10)

        iter = 0
        imaxiter = 10

        do while (iter .LT. imaxiter)
          axnovo=axme-f(axme)*(axme-axma)/(f(axme)-f(axma))
          asec(iter + 1) = axnovo

          if (abs(axnovo - axme) .LE. atolerancia) exit

          axma = axme
          axme = axnovo
          iter = iter + 1
        end do

        do while (iter .LT. 10)
          asec(iter+1) = axnovo
          iter = iter + 1
        end do
      end subroutine secante


      real*8 function f(ax)
        real*8 ax
        f = 27*ax**3 - 522*ax**2 + 3003*ax - 4508
      end function f

      real*8 function df(ax)
        real*8 ax
        df = 81*ax**2 - 1044*ax + 3003
      end function df