      program ex4
        integer M, N

        N = 10000

        write(*, *) 'Digite o número de andarilhos: '; read(*, *) M

        call random_walk_bi(M, N)

      end program ex4

      subroutine random_walk_bi(M, N)
        real*8 p, p2, p3
        integer positionX, positionY
        integer grid(-N:N, -N:N)

        p = 0.25
        p2 = 2*p
        p3 = 3*p

        do i = 1, M
          positionX = 0
          positionY = 0
          do j = 1, N
            pos = rand()

            if (pos .lt. p) then
              positionX = positionX + 1
            else if (pos .lt. p2) then
              positionX = positionX - 1
            else if (pos .lt. p3) then
              positionY = positionY + 1
            else
              positionY = positionY - 1
            end if
            grid(positionX, positionY) = grid(positionX, positionY) + 1
          end do
        end do

        call calculate_entropy(M, N, grid)

      end subroutine random_walk_bi

      subroutine calculate_entropy(M, N, grid)
        integer M, N, grid(-N:N, -N:N)
        real*8 entropy, p_i

        entropy = 0.0

        do i = -N, N
          do j = -N, N
            if (grid(i, j) .gt. 0) then
              p_i = dble(grid(i, j)) / dble(M * N)
              entropy = entropy - p_i * log(p_i)
            end if
          end do
        end do

        write(*, *) 'Entropia: ', entropy
      end subroutine calculate_entropy