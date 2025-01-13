      program ex3
        N = 1000000
        write(*, *) 'Digite o número de andarilhos: '; read(*, *) M

        call random_walk_bi(M, N)
      end program ex3

      subroutine random_walk_bi(M, N)
        real*8 p, p2, p3, pos, mediaX, mediaY, deltaR, mediaR2
        dimension ipositionX(M), ipositionY(M)

        p = 0.25
        p2 = 2 * p
        p3 = 3 * p
        deltaR = 0.0
        mediaX = 0.0
        mediaY = 0.0
        mediaR2 = 0.0

        open(1, file="saida-3-#14668849.out")

        do i = 1, M
          ipositionX(i) = 0
          ipositionY(i) = 0
          do j = 1, N
            pos = rand()
            if (pos .lt. p) then
              ipositionX(i) = ipositionX(i) + 1
            else if (pos .lt. p2) then
              ipositionX(i) = ipositionX(i) - 1
            else if (pos .lt. p3) then
              ipositionY(i) = ipositionY(i) + 1
            else
              ipositionY(i) = ipositionY(i) - 1
            end if
        
            write(1, *) ipositionX(i), ipositionY(i)
          end do

          mediaX = mediaX + ipositionX(i)
          mediaY = mediaY + ipositionY(i)

          mediaR2 = mediaR2 + (ipositionX(i)**2 + ipositionY(i)**2)
        end do

        mediaX = mediaX / M
        mediaY = mediaY / M
        mediaR2 = mediaR2 / M
        deltaR = mediaR2 - (mediaX**2 + mediaY**2)

        write(*, *) 'Média X: ', mediaX
        write(*, *) 'Média Y: ', mediaY
        write(*, *) 'Delta^2: ', deltaR

      end subroutine random_walk_bi