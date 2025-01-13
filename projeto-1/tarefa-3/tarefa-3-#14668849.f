      program ex3
        real*8, allocatable :: numbers(:)
        real*8 temp
        integer m, n, i

        open(unit=1, file='entrada-3-#14668849.in')
        open(unit=2, file='saida-3-#14668849.out')

        !Número de valores no arquivo
        n = 0
        do
          read(1,*, end=100)
          n = n + 1
        end do

100     continue

        !Alocar memória para os números - Array
        allocate(numbers(n))

        !Voltar para o início do arquivo
        rewind(1)

        !Ler os valores do arquivo
        do i = 1, n
          read(1,*) numbers(i)
        end do

        !Ler o valor de M
        write(*, 130); read(*,*) m

        !Verificar se M é menor ou igual ao número de valores
        if (m > n) then
          print*, 'M deve ser menor ou igual ao número de valores'
          stop
        end if

        !Ler e ordenar os primeiros M números menores do arquivo
        do i = 1, n - 1
          do j = 1, n - 1
            if (numbers(j) > numbers(j + 1)) then
              temp = numbers(j)
              numbers(j) = numbers(j + 1)
              numbers(j + 1) = temp
            end if
          end do
        end do

        !Escrever os M números menores no arquivo de saída
        write(2, *) 'Valor de M = ', m
        do i = 1, m
          write(2, '(F11.9)') numbers(i)
        end do

        write(*, 120) n

120     format('O número de valores do arquivo é: ' i5)
130     format('Digite o valor de M: ' $)

        close(1); close(2)
      end program ex3