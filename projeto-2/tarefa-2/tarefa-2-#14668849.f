      program ex2
        integer M, N

        N = 1000
        write(*, *) 'Digite o número de andarilhos: '; read(*, *) M
        
        call random_walk(M, N)
      end program ex2

      subroutine random_walk(M, N)
        real*8 p, media, media2
        integer position
        dimension histogram(-N:N)

        open(1, file="saida-2-#14668849.out")

        p = 0.5
        histogram = 0
        
        do i = 1, M
          position = 0
          do j = 1, N
            if (rand() .GT. p) then
              position = position + 1
            else
              position = position - 1
            end if
          end do
          histogram(position) = histogram(position) + 1
        end do

        media = 0.0
        media2 = 0.0
        do i = -N, N
          if (histogram(i) .gt. 0) then
            media = media + i * histogram(i) 
            media2 = media2 + i * i * histogram(i) 
          end if
          write(1, *) i, histogram(i) 
        end do

        write(*, *) 'Média: ', media / M
        write(*, *) 'Média^2: ', media2 / M

      end subroutine random_walk