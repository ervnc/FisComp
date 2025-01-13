      program ex1
        real raio1, raio2
        real*8 pi

        pi = acos(-1D0)

        write(*, 1); read(*,*) raio1

        write(*, 2); read(*,*) raio2
1       format('Digite o valor do raio interno: ' $)
2       format('Digite o valor do raio externo: ' $)
      
        write(*,*)'A área do torus é: ', 4*(pi**2)*raio1*raio2
        write(*,*)'O volume do torus é: ', 2*(pi**2)*(raio1**2)*raio2

      end program ex1