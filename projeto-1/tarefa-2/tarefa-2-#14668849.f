      program ex2
        dimension v1(3), v2(3), v3(3)
        real crossI, croosJ, crossK, norm_cross, area_lateral, volume, dot
        real crossV2I, crossV2J, crossV2K, crossV3I, crossV3J, crossV3K
        real normV1, normV2

        write(*, *)'Digite os valores do vetor 1: '; read(*,*) v1
        write(*, *)'Digite os valores do vetor 2: '; read(*,*) v2
        write(* ,*)'Digite os valores do vetor 3: '; read(*,*) v3

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

        !Calcular produto vetorial de v2 X v3
        crossV2I = v2(2)*v3(3) - v2(3)*v3(2)
        crossV2J = v2(3)*v3(1) - v2(1)*v3(3)
        crossV2K = v2(1)*v3(2) - v2(2)*v3(1)

        !Calcular produto vetorial de v3 X v1
        crossV3I = v3(2)*v1(3) - v3(3)*v1(2)
        crossV3J = v3(3)*v1(1) - v3(1)*v1(3)
        crossV3K = v3(1)*v1(2) - v3(2)*v1(1)

        !Calcular a norma dos vetores
        normV1 = sqrt(crossV2I**2 + crossV2J**2 + crossV2K**2)
        normV2 = sqrt(crossV3I**2 + crossV3J**2 + crossV3K**2)

        !Calcular a área lateral
        area_lateral = 2*(normV1 + normV2 + norm_cross)

        !Calcular o volume
        volume = abs(dot)

        write(*,*) 'A área lateral é: ', area_lateral
        write(*,*) 'O volume é: ', volume
      end program ex2