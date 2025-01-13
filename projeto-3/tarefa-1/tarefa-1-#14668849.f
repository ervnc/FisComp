      program ex1
        real*8 x, h, f, f2f, f2t, f3s, f5s, ff3s, ff5s, fExato, f2Exato

        x = 0.5
        write(1, 110) "h","f'2s","f'2t","f'3s","f'5s","f''3s","f''5s" 
        write(1, 120)

        fExato = -2.80235111004d0
        f2Exato = 4.53476386947d0
        
        do i = 1, 12
          h = 5.0d0 ** (-i)

          f2f = (f(x+h) - f(x)) / h

          f2t = (f(x) - f(x-h)) / h

          f3s = (f(x+h) - f(x-h))/(2*h)

          f5s = (f(x-(2*h)) - 8*f(x-h) + 8*f(x+h) - f(x+(2*h)))/(12*h)

          ff3s = (f(x+h) - 2*f(x) + f(x-h))/(h**2)

          ff5s = -f(x-(2*h))+16*f(x-h)-30*f(x)+16*f(x+h)-f(x+(2*h))
          ff5s = ff5s / (12*(h**2))

          write(1, 100) h, f2f, f2t, f3s, f5s, ff3s, ff5s
        end do

        write(1, 120)
        write(1, 13)"Exato",fExato,fExato,fExato,fExato,f2Exato,f2Exato

100     format(f20.12, 9("|", f20.11), "|")
110     format(a20, 9("|", a20), "|")
120     format(147("-"))
13      format(a20, 20("|", f20.11), "|")

      end program ex1

      real*8 function f(x)
        real*8 x
        f = exp(x**2)*(1/tan(2*x))
      end function f