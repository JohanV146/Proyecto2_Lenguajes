module InfoBici where
import System.IO
-- ======== Registro de parqueo ==================================================================================
type Codigo = String
type Tipo = String
data Bicicleta = Bicicleta Codigo Tipo;
-- ===============================================================================================================
-- ======== Constructor de data Bicicleta y gets ===================================================================
crearBicicleta(elemento) = Bicicleta (elemento!!0) (elemento!!1)
getCodigo (Bicicleta codigo _) = codigo;
getTipo (Bicicleta _ tipo) = tipo;

-- ===============================================================================================================
-- ===============================================================================================================
type Idbicicleta = String
type Idparqueo = Int
data UbicacionBici = UbicacionBici Idbicicleta Idparqueo;
-- ===============================================================================================================
-- ======== Constructor de data ubicacion bicicleta y gets ===================================================================
crearUbicacionBici(elemento) = UbicacionBici (elemento!!0) (read (elemento!!1) :: Int)
getIdbicicleta (UbicacionBici idbicicleta _) = idbicicleta;
getIdparqueo (UbicacionBici _ idparqueo) = idparqueo;
-- ===============================================================================================================
-- ===============================================================================================================

-- ======== Convierte a String los datos de una data parqueo =====================================================
showBicicleta :: Bicicleta -> [Char]
showBicicleta bicicleta =
            let
                codigo = getCodigo(bicicleta)
                tipo = getTipo(bicicleta)
            in 
                "Codigo: " ++ codigo ++ "\n" ++
                "Tipo: " ++ tipo ++ "\n"

-- ======== Muestra todas las data usuarios de una lista ===========================================
showBicicletas:: [Bicicleta] -> IO ()
showBicicletas [] = print("")
showBicicletas lista =
            do
                putStrLn(showBicicleta (head lista))
                showBicicletas (tail lista)

-- ======== Split por comas ========================================================================
separarPorComas :: ([Char], [Char]) -> [[Char]]
separarPorComas (cadena, temp) =
    if cadena == "" then [temp] else
        if (head cadena) == (head ",") then
            [temp] ++ separarPorComas ((tail cadena), "")
            else
                separarPorComas((tail cadena), temp++[(head cadena)])

-- ======== Separa por comas una linea y lo convierte en data tipo parqueo =========================
separarElementosB :: [[Char]] -> [Bicicleta]
separarElementosB lista =
    if lista == [] then
        []
    else
        [crearBicicleta(separarPorComas((head lista), ""))] ++ separarElementosB (tail lista)

-- ==================================================================================================
separarElementosBNew :: [[Char]] -> [UbicacionBici]
separarElementosBNew lista =
    if lista == [] then
        []
    else
        [crearUbicacionBici(separarPorComas((head lista), ""))] ++ separarElementosBNew (tail lista)

-- ======== Convierte en una lista un texto, parte el texto por cambios de lineas ===================
convierteAlineas :: String -> [String]
convierteAlineas texto = lines texto

-- ======== Lee todo el contenido de archivo, los convierte en lineas =================================
-- ======== luego parsea por comas las lineas y genera elementos parqueos =============================
leerArchivoB :: FilePath -> IO [Bicicleta]
leerArchivoB archivo = do
    contenido <- readFile archivo
    let bicicletas = separarElementosB (convierteAlineas contenido)
    return bicicletas

-- ====================================================================================================
leerArchivoBNew :: FilePath -> IO [UbicacionBici]
leerArchivoBNew archivo = do
    contenido <- readFile archivo
    let ubicacionBicis = separarElementosBNew (convierteAlineas contenido)
    return ubicacionBicis

