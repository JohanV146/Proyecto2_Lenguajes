data Empresa = Empresa
    { 
        nombreEmpresa :: String,
        sitioWeb :: String,
        contacto :: String,
        tarifaPedal :: Int,
        tarifaElectrico :: Int,
        tarifaGasolina :: Int
    }

-- Función para validar si una cadena es un número de 8 dígitos
esNumeroValido :: String -> Bool
esNumeroValido s = length s == 8 && all (\c -> c >= '0' && c <= '9') s

-- Función para registrar la información de la empresa
registrarEmpresa :: IO Empresa
registrarEmpresa = do
    putStrLn "Ingrese el nombre de la empresa:"
    nombre <- getLine
    putStrLn "Ingrese el sitio web de la empresa:"
    web <- getLine
    putStrLn "Ingrese el número de contacto (8 dígitos):"
    contactoInfo <- getLine
    if esNumeroValido contactoInfo
        then do
            putStrLn "Ingrese la tarifa por kilómetro pedal (TR):"
            tarifaTR <- readLn
            putStrLn "Ingrese la tarifa por kilómetro eléctrico (AE):"
            tarifaAE <- readLn
            putStrLn "Ingrese la tarifa por kilómetro gasolina (AG):"
            tarifaAG <- readLn

            let empresa = Empresa
                    { 
                        nombreEmpresa = nombre,
                        sitioWeb = web,
                        contacto = contactoInfo,
                        tarifaPedal = tarifaTR,
                        tarifaElectrico = tarifaAE,
                        tarifaGasolina = tarifaAG
                    }

            putStrLn "Información de la empresa registrada con éxito."
            return empresa
        else do
            putStrLn "Número de contacto no válido. Debe tener 8 dígitos."
            registrarEmpresa