module Practica2 where


--   a) //////////////////////////////////////////////////////////////////////////////////
    baseD n base | n < 1 = 0
                 | otherwise = ((baseD (div n base) base)*10) + (mod n base)
--   b) //////////////////////////////////////////////////////////////////////////////////
    dijitoI x | x < 10 = 0
              | otherwise = (dijitoI(div x 10) * 10) + (mod x 10)
--   c) //////////////////////////////////////////////////////////////////////////////////  
    nueves x | x == 1 = 9
             |otherwise = (nueves (x-1) * 10) + 9
--   d) //////////////////////////////////////////////////////////////////////////////////  
    rotarI x n = u + (p * m)
          where 
            u = ultimos x (nueves n)

            p = primeros x  (nueves n)

            m = multiplicar u 

    ultimos x n | x <= n = x 
                | otherwise = ultimos (div x 10) n 

    primeros x n| x <= n = 0
                | otherwise = ((primeros (div x 10) n)*10)+ mod x 10   

    multiplicar x | x < 1 = 1
                  | otherwise = (multiplicar (div x 10)  ) * 10 

--   e) ////////////////////////////////////////////////////////////////////////////////// 


    menosS x = pms 
          where 

            dijitoM = dijitoMayor x

            pms = menosSignificativa x dijitoM
    dijitoMayor x | x < 10 = x
                  | otherwise = max (mod x 10) (dijitoMayor (div x 10))
                  
    menosSignificativa x n| x < 10 = if x == n then 0 else 1
                          | otherwise = ((menosSignificativa (div x 10) n * menosSignificativa (mod x 10) n)*10) + mod x 10
    

--   f) ////////////////////////////////////////////////////////////////////////////////// 

    encriptar x = ((moverValores (div (baseD x 9) 100000000))*100000000) 
                            + moverValores (mod (baseD x 9) 100000000)


    moverValores n = ultimosCuatro n + ((primerosCuatro n )* 10000)

    ultimosCuatro x | x <= 9999 = x
                    | otherwise = ultimosCuatro (div x 10) 

    primerosCuatro x | x <= 9999 = 0
                     | otherwise = ((primerosCuatro (div x 10)) * 10) + mod x 10
                  

--   f) ////////////////////////////////////////////////////////////////////////////////// 

    convertirBaseDiez x n| x < 10 = x * (potencia 9 n)
                         | otherwise = (convertirBaseDiez (div x 10) (n+1))+((mod x 10)*(potencia 9 n))
    
    potencia x n| n < 1 = 1
                | otherwise = x * potencia x (n-1)

    girarValores x= ((moverValores (div x 100000000))*100000000) 
                            + moverValores (mod x 100000000)

    desencriptar x = convertirBaseDiez (girarValores x) 0

    