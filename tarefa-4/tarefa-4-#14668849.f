      program ex4
        real*8 x, precision, cosSeries, term, cosFunction
        integer n, factorial

        !Ler o valor de X
        write(*, 100); read(*,*) x

        precision = 1.0D-5
        cosSeries = 1.0D0
        term = 1.0D0
        n = 0
        factorial = 1

        do while (abs(term) > precision)
          n = n + 2
          factorial = factorial * n * (n - 1)
          term = (-1)**(n/2) * (x**n) / factorial
          cosSeries = cosSeries + term
        end do

        cosFunction = cos(x)

100     format('Digite o valor de x: ' $)

      end program ex4