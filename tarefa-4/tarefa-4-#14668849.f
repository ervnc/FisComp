      program ex4
        real*8 x, precision, cosSeries, term, pi
        integer n

        !Ler o valor de X
        write(*, 100); read(*,*) x

        precision = 1.0D-17
        cosSeries = 1.0D0
        term = 1.0D0
        n = 0
        factorial = 1
        pi = acos(-1.0D0)

        x = mod(x, 2.0D0*pi)

        do while (abs(term) > precision)
          n = n + 2
          factorial = factorial * n * (n - 1)
          term = (-1)**(n/2) * (x**n) / factorial
          cosSeries = cosSeries + term
        end do

100     format('Digite o valor de x: ' $)

        write(*, *) 'Cos (série): ', cosSeries
        write(*, *) 'Cos: ', dcos(x)

      end program ex4