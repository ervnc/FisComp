      program ex5
        integer perm(20000), n

        open(unit=1, file='entrada-5-#14668849.in')
        open(unit=2, file='saida-5-#14668849.out')

        ! Pegar maior número
        ibigger = 1
        do
          read(1, *, end=100) icurrent
          if (icurrent .gt. ibigger) then
            ibigger = icurrent
          end if
        end do
100     continue

        ! Número máximo para as permutações
        ibigger = ibigger + 1

        !Tamanho da matriz
        n = ibigger

        !Permutação inicial
        do i = 1, ibigger
          perm(i) = i
        end do

        call permute(perm, ibigger, ibigger, permute)
        close(2)
        close(1)
      end program ex5

      !Função recursiva para a permutação
      subroutine permute(perm, n, original_n, dumperm)
        integer perm(*), n, original_n
        external dumperm
        integer i, temp, paridade

        if (n .EQ. 1) then
          paridade = 1
          do i = 1, original_n - 1
            do j = 1 + 1, original_n
              if (perm(i) .GT. perm(j)) then 
                paridade = -paridade
              end if
            end do
          end do
          write(2, '(10i2, i2)') (perm(i), i = 1, original_n), paridade
        else
          do i = 1, n
            call dumperm(perm, n - 1, original_n, dumperm)
            if (mod(n, 2) .EQ. 0) then
              temp = perm(i)
              perm(i) = perm(n)
              perm(n) = temp
            else
              temp = perm(1)
              perm(1) = perm(n)
              perm(n) = temp
            end if
          end do
        end if
        return
      end subroutine permute