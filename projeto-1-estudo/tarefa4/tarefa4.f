      program ex4
        real*8 cosSerie, precision, x, pi, term, factorial
        integer n

        write(*, *) 'Digite o valor de x: '; read(*, *) x

        pi = acos(-1.0d0)
        precision = 1.0d-5
        term = 1.0d0
        factorial = 1.0
        cosSerie = 1.0d0
        n = 0

        x = mod(x, -2.0d0*pi)

        do while (abs(term) > precision)
          n = n + 2
          factorial = factorial * n * (n - 1)
          term = (-1)**(n/2) * (x**n) / factorial
          cosSerie = cosSerie + term
        end do

        write(*, *) cosSerie
        write(*, *) dcos(x)

      end program ex4