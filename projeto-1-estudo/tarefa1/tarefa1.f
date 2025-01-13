      program ex1
        real*8 r1, r2, area, volume, pi

        pi = acos(-1d0)

        write(*, *) 'Digite o raio interno: '; read(*, *) r1
        write(*, *) 'Digite o raio externo: '; read(*, *) r2

        area = 4 * pi**2 * r2 * r1
        volume = 2 * pi**2 * r2 * r1**2
        
        write(*, *) 'Área do torus: ', area
        write(*, *) 'Volume do torus: ', volume  
      end program ex1