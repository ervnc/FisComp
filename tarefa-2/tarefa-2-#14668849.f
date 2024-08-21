      program ex2
        dimension v1(3), v2(3), v3(3), v4(3)
        real crossI, croosJ, crossK, norm_cross, crossV4I, croosV4J, crossV4K, area_lateral, volume, dot

        write(*, *)'Digite os valores do vetor 1: '; read(*,*) v1
        write(*, *)'Digite os valores do vetor 2: '; read(*,*) v2
        write(* ,*)'Digite os valores do vetor 3: '; read(*,*) v3

        v4 = v1 - v2

        !Produto vetorial entre v1 X v2
        crossI = v1(2)*v2(3) - v1(3)*v2(2)
        croosJ = v1(1)*v2(3) - v1(3)*v2(1)
        crossK = v1(1)*v2(2) - v1(2)*v2(1)

        norm_cross = sqrt(crossI**2 + croosJ**2 + crossK**2)

        !Verificar se v1 e v2 são paralelos
        if (norm_cross .EQ. 0.0) then
          write(*,*) 'Os vetores v1 e v2 são paralelos'
          stop
        end if

        !Verificar se v3 está no mesmo plano de v1 e v2
        dot = v3(1)*crossI + (-1*v3(2)*croosJ) + v3(3)*crossK
        if (dot .EQ. 0.0) then
          write(*,*) 'O vetor v3 está no mesmo plano que v1 e v2'
          stop
        end if

        !Calcular o produto vetorial de v4 X v3
        crossV4I = v4(2)*v3(3) - v4(3)*v3(2)
        croosV4J = v4(1)*v3(3) - v4(3)*v3(1)
        crossV4K = v4(1)*v3(2) - v4(2)*v3(1)

        !Calcular a área lateral
        area_lateral = sqrt(crossV4I**2 + croosV4J**2 + crossV4K**2)

        !Calcular o volume
        volume = abs(dot)

        write(*,*) 'A área lateral é: ', area_lateral
        write(*,*) 'O volume é: ', volume
      end program ex2