      program ex2
        real*8 ah, api, a, ab, atrap, asimp, aboo, aexato
        real*8 trapezio, simpson, boole

        api = acos(-1.0D0)
        a = 0.0D0
        ab = 2.0D0*api

        aexato = 0.49906627797d0

        write(1, 110) "2*pi X h^-1","Trápezio","Simpson","Boole"
        write(1, 120)
        
        do i = 2, 13
          ah = (ab - a) / (2.0d0 ** i)
          in = 2 ** i

          atrap = trapezio(a, ab, ah)
          asimp = simpson(a, ab, ah)
          aboo = boole(a, ab, ah)

          write(1, 100) in, atrap, asimp, aboo
        end do

        write(1, 120)
        write(1, 13) "Exato", aexato, aexato, aexato

100     format(i15, 4("|", f18.11), "|")
110     format(a15, 4("|", a18), "|")
120     format(73("-"))
13      format(a15, 4("|", f18.11), "|")
      end program ex2

      real*8 function trapezio(a, ab, ah)
        real*8 a, ab, ah, asoma, f
        integer in

        in = int((ab - a) / ah)
        asoma = (f(a) + f(ab)) * 0.5d0

        do i = 1, in - 1
          asoma = asoma + f(a + i*ah)
        end do

        trapezio = asoma * ah
      end function trapezio

      real*8 function simpson(a, ab, ah)
        real*8 a, ab, ah, asoma, f
        integer in

        in = int((ab - a) / ah)
        asoma = f(a) + f(ab)

        do i = 1, in - 1
          if (mod(i, 2) == 0) then
            asoma = asoma + 2.0d0 * f(a + i*ah)
          else
            asoma = asoma + 4.0d0 * f(a + i*ah)
          end if
        end do

        simpson = asoma * ah / 3.0d0
      end function simpson

      real*8 function boole(a, ab, ah)
        real*8 a, ab, ah, asoma, f
        integer in

        in = int((ab - a) / ah)
        asoma = 0.0d0

        do i = 2, in - 2, 4
          asoma = asoma + 7.0d0 * (f(a + (i-2)*ah) + f(a + (i+2)*ah))
          asoma = asoma + 32.0d0 * (f(a + (i-1)*ah) + f(a + (i+1)*ah))
          asoma = asoma + 12.0d0 * f(a + i*ah)
        end do

        boole = asoma * 2.0d0 * ah / 45.0d0
      end function boole

      real*8 function f(ax)
        real*8 ax
        f = exp(-ax)*cos(ax)
      end function f