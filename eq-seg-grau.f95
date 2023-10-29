program eq_seg_grau

    implicit none
    
    real :: a, b, c, delta, x1, x2, complex_part, real_part

    print *, "Este programa calcula as raizes de uma equacao de segundo grau"
    print *, "A equacao deve ser da forma ax^2 + bx + c = 0"
    print *, "Entre com os coeficientes a, b e c da equacao de segundo grau"
    read *, a, b, c

    delta = b**2 - 4*a*c

    if ( delta > 0 ) then
        print *, "A equacao possui duas raizes reais"
        x1 = (-b + sqrt(delta)) / (2*a)
        x2 = (-b - sqrt(delta)) / (2*a)
        write (*,*) "x1 = ", x1
        write (*,*) "x2 = ", x2

    else if ( delta == 0 ) then
        print *, "A equacao possui uma raiz real"
        x1 = -b / (2*a)
        write (*,*) "x1 = ", x1

    else
        print *, "A equacao possui raizes complexas"
        real_part = -b / (2*a)
        complex_part = sqrt(abs(delta)) / (2*a)
        write (*,*) "x1 = ", real_part, " + ", complex_part, "i"
        write (*,*) "x2 = ", real_part, " - ", complex_part, "i"
    end if

end program eq_seg_grau
