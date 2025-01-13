      program ex7
        integer d
        real*8 volume, raio

        open(1, file='saida-7-#14668849.out')

        write(*, *) 'Digite a dimensão: '; read(*, *) d
        write(*, *) 'Digite o raio: '; read(*, *) raio

        do i = 1, d
          call volume_esfera_teorico(i, raio, volume)
          write(1, *) 'Dimensão: ', i, ' Volume: ', volume
        end do

        close(1)

        call grafico()
      end program ex7 

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

      subroutine grafico()
        integer i
        real*8 result

        open(2, file='saida-7b-#14668849.out')

        do i = 0, 25
          call volume_esfera_teorico(i, 0.9d0, result)
          call volume_esfera_teorico(i, 1.0d0, result)
          call volume_esfera_teorico(i, 1.1d0, result)
          write(2, *) i, result
        end do

        close(2)
      end subroutine grafico