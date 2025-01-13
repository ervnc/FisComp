      program ex2
        real*8 v1(3), v2(3), v3(3), cross(3), norm, dot, area_lateral
        real*8 crossv1v2(3),crossv2v3(3), crossv1v3(3), normv2v3, normv1v3

        write(*, *) 'Digite o primeiro vetor: '; read(*, *) v1
        write(*, *) 'Digite o segundo vetor: '; read(*, *) v2
        write(*, *) 'Digite o terceiro vetor: '; read(*, *) v3

        !Produto vetorial entre v1 X v2
        call norm_cross(v1, v2, cross, norm)
        crossv1v2 = cross
        if (norm .EQ. 0.0) then
          write(*, *) 'Os vetores v1 e v2 não podem ser paralelos.'
        end if

        !Verificar se V3 está no mesmo plano que V1 e V2
        dot = v3(1)*crossv1v2(1)+v3(2)*crossv1v2(2)+v3(3)*crossv1v2(3)
        if (dot .EQ. 0.0) then
          write(*, *) 'O vetor v3 está no mesmo plano que v1 e v2.'
        end if

        !Produto vetorial entre v2 x v3
        call norm_cross(v2, v3, cross, norm)
        crossv2v3 = cross
        normv2v3 = norm

        !Produto vetorial entre v1 X v3
        call norm_cross(v1, v3, cross, norm)
        crossv1v3 = cross
        normv1v3 = norm

        area_lateral = 2 * (norm + normv1v3 + normv2v3)

        write(*, *) 'A área lateral é: ', area_lateral
        write(*, *) 'O volume é: ', abs(dot)
        
      end program ex2

      subroutine norm_cross(v1, v2, cross, norm)
        real*8 v1(3), v2(3), cross(3), norm
        real*8 crossI, crossJ, crossK

        crossI = v1(2)*v2(3) - v1(3)*v2(2)
        crossJ = -1*(v1(1)*v2(3) - v1(3)*v2(1))
        crossK = v1(1)*v2(2) - v1(2)*v2(1) 

        cross(1) = crossI
        cross(2) = crossJ
        cross(3) = crossK

        norm = sqrt(crossI**2 + crossJ**2 + crossK**2)
      end subroutine