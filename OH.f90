program oh 

    use, intrinsic  :: iso_fortran_env, only : wp =>real64
    implicit none
    
     character(len=30) :: nomFichier = "OH.dat"
     real(wp),parameter :: w0 = 1.0_wp 
     real(wp), parameter :: x0 = 1.0_wp
     real(wp), parameter :: v0 = 0.0_wp 
     real(wp),parameter :: tmin = 0.0_wp 
     real(wp),parameter :: tmax =50.0_wp 
     real(wp),parameter :: dt = 0.05_wp 
     integer, parameter :: Nmax = 1000
     real(wp) :: vEI,vEE,vEE_1, tn, vEI_1, xtheorique, XEI_1,XEE_1,XEI,XEE
     integer :: n , io 
     
     
    !ouverture du fichier de données 
    open(newunit=io, file=trim(nomFichier))
    !ecriture de la premiere ligne du fichier -> conditions initiales 
    write(io,*) tmin,'   ',x0,'   ', x0,'    ',x0
    
    XEE_1 = x0 
    XEI_1 = x0 
    
    VEE_1 = v0 
    vEI_1 = v0 
    
     do n=1, Nmax 
       ! calcul de l'instant tn 
       tn = tmin + n*dt 
       ! implementation des equations d'évolution de l'algo d'eulervn = vn_1 - w0**2*xn*dt
       XEE =  xEE_1 +vEE_1*dt
       vEE = vEE_1 - w0**2*xEE_1*dt 
       
       vEI = vEI_1 - w0**2*xEI_1*dt 
       XEI = xEI_1 + vEI*dt
     
       !calcul de la hauteur théorique 
       xtheorique = x0*cos(w0*tn) 
       !enregistrement des données 
       write(io,*) tn, '   ', XEE_1, '   ', XEI_1, '    ', xtheorique
       !substitutions pour preparer le pas d'apres
       xEE_1=xEE
       XEI_1=XEI
       
       vEE_1 = vEE
       vEI_1 = vEI      
    end do
    
     !fermeture du fichier de données
    close(io) 
    
     


end program oh 
