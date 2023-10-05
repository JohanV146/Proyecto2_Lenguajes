module InfoEmpresa where

-- ======== Registro de empreza ==================================================================================
type NombreEmpresa = String
type SitioWeb = String
type Contacto = String
type TarifaPedal = Int
type TarifaElectrico = Int
type TarifaGasolina = Int

data Empresa = Empresa NombreEmpresa SitioWeb Contacto TarifaPedal TarifaElectrico TarifaGasolina;
-- ===============================================================================================================
-- ======== Constructor de data Empreza y gets ===================================================================
crearEmpresa(elemento) = Empresa (elemento!!0) (elemento!!1) (elemento!!2) (read (elemento!!3) :: Int) (read (elemento!!4) :: Int) (read (elemento!!5) :: Int)
getNombreEmpresa (Empresa nombre _ _ _ _ _) = nombre;
getSitioWeb (Empresa _ sitioWeb _ _ _ _) = sitioWeb;
getContacto (Empresa _ _ contacto _ _ _) = contacto;
getTarifaPedal (Empresa _ _ _ tarifaPedal _ _) = tarifaPedal;
getTarifaElectrico (Empresa _ _ _ _ tarifaElectrico _) = tarifaElectrico;
getTarifaGasolina (Empresa _ _ _ _ _ tarifaGasolina) = tarifaGasolina;

-- ======== Convierte a String los datos de una data Empresa =====================================================
showEmpresa :: Empresa -> [Char]
showEmpresa empresa =
            let
                nombre = getNombreEmpresa(empresa)
                web = getSitioWeb(empresa)
                contac = getContacto(empresa)
                pedal = getTarifaPedal(empresa)
                electrico = getTarifaElectrico(empresa)
                gasolina = getTarifaGasolina(empresa)
            in 
                "Nombre de la Empresa: " ++ nombre ++ "\n" ++
                "Sitio Web: " ++ web ++ "\n" ++
                "Contacto: " ++ contac ++ "\n" ++
                "Tarifa para Pedal: " ++ show pedal ++ "\n" ++
                "Tarifa para Electrico: " ++ show electrico ++ "\n" ++
                "Tarifa para Gasolina: " ++ show gasolina

-- ======== Muestra todas las data Empresa de una lista ===========================================
showEmpresas :: [Empresa] -> IO ()
showEmpresas lista =
                putStrLn(showEmpresa (head lista))

-- ======== Split por comas ========================================================================
separarPorComas :: ([Char], [Char]) -> [[Char]]
separarPorComas (cadena, temp) =
    if cadena == "" then [temp] else
        if (head cadena) == (head ",") then
            [temp] ++ separarPorComas ((tail cadena), "")
            else
                separarPorComas((tail cadena), temp++[(head cadena)])

-- ======== Separa por comas una linea y lo convierte en data tipo persona =========================
separarElementos :: [[Char]] -> [Empresa]
separarElementos lista =
    if lista == [] then
        []
    else
        [crearEmpresa(separarPorComas((head lista), ""))] ++ separarElementos (tail lista)

-- ======== Convierte en una lista un texto, parte el texto por cambios de lineas ===================
convierteAlineas :: String -> [String]
convierteAlineas texto = lines texto

-- ======== Lee todo el contenido de archivo, los convierte en lineas ================================
-- ======== luego parsea por comas las lineas y genera elementos persona =============================
leerArchivo :: FilePath -> IO [Empresa]
leerArchivo archivo = do
    contenido <- readFile archivo
    let empresas = separarElementos (convierteAlineas contenido)
    return empresas