      program ex6
        integer m, d
        real*8 volume, volumeTeorico, raio

        write(*, *) 'Digite a dimensão: '; read(*, *) d
        write(*, *) 'Digite o número de pontos: '; read(*, *) m
        write(*, *) 'Digite o raio: '; read(*, *) raio

        call volume_esfera_mc(d, m, raio, volume)
        call volume_esfera_teorico(d, raio, volumeTeorico)
        write(*, *) 'Volume (rand): ', volume
        write(*, *) 'Volume Teórico: ', volumeTeorico

      end program ex6

      subroutine volume_esfera_mc(d, m, raio, volume)
        integer d, m, n_dentro, i, j
        real*8 volume, x, soma_quadrados, raio
        n_dentro = 0
        do i = 1, m
          soma_quadrados = 0.0d0
          do j = 1, d
            x = rand()
            soma_quadrados = soma_quadrados + (x)**2
          end do
          if (soma_quadrados .LE. 1.0d0) then
            n_dentro = n_dentro + 1
          end if
        end do
        
        volume = (2.0d0*raio)**d * (n_dentro / real(m))
      end subroutine volume_esfera_mc

      subroutine volume_esfera_teorico(d, raio, volumeTeorico)
        integer d
        real*8 raio, volumeTeorico, pi, gamma_d, ad

        ad = d
        pi = acos(-1D0)
        ad = ad/2.0d0 + 1.0d0
        gamma_d = 1

        do while (ad .gt. 1.D0)
          ad = ad - 1
          gamma_d = gamma_d * ad
        end do
        
        volumeTeorico = (pi**(d / 2) / gamma_d) * (raio**d)
      end subroutine volume_esfera_teorico