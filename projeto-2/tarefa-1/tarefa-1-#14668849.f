      program ex1
        call media_gerada()
      end program ex1

      subroutine media_gerada
        real*8 x, soma1, soma2, soma3, soma4
        integer N, i

        N = 1000000000

        soma1 = 0.0
        soma2 = 0.0
        soma3 = 0.0
        soma4 = 0.0

        do i = 1, N
          x = rand()

          soma1 = soma1 + x
          soma2 = soma2 + x**2
          soma3 = soma3 + x**3
          soma4 = soma4 + x**4
        end do

        print*, 'Média de x^1: ', soma1/N
        print*, 'Média de x^2: ', soma2/N
        print*, 'Média de x^3: ', soma3/N
        print*, 'Média de x^4: ', soma4/N
      end subroutine media_gerada