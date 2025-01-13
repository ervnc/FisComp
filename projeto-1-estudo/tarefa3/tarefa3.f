      program ex3
        integer N, M

        open(1, file="entrada-3-#14668849.in")
        open(2, file="saida3.out")

        call count_lines_file(N)
        write(*, *) "Número de linhas no arquivo: ", N

        write(*, *) "Digite M: "; read(*, *) M 
        call sort_file(N, M)

        close(1); close(2)

      end program ex3

      subroutine count_lines_file(N)
        integer N

        N = 0
        do
          read(1, *, end=100)
          N = N + 1
        end do

100     continue
      end subroutine

      subroutine sort_file(N, M)
        integer N, M
        real*8 array(1001), temp

        rewind(1)

        if (M > N) then
          write(*, *) 'O valor de M deve ser menor que N'
          stop
        end if

        do i = 1, N
          read(1, *) array(i)
        end do

        do i = 1, N - 1
          do j = 1, N - 1
            if (array(j) > array(j + 1)) then
              temp = array(j)
              array(j) = array(j + 1)
              array(j + 1) = temp
            end if
          end do
        end do

        do i = 1, M
          write(2, *) array(i)
        end do
      end subroutine