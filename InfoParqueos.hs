module InfoParqueos where
import Control.Monad (forM_)
import System.IO
-- ======== Registro de parqueo ==================================================================================
type Id = Int
type Nombre = String
type Direccion = String
type Provincia = String
type UbicacionX = String
type UbicacionY = String

data Parqueo = Parqueo Id Nombre Direccion Provincia UbicacionX UbicacionY;
-- ===============================================================================================================
-- ======== Constructor de data parqueo y gets ===================================================================
crearParqueo(elemento) = Parqueo (read (elemento!!0) :: Int) (elemento!!1) (elemento!!2) (elemento!!3) (elemento!!4) (elemento!!5)
getId (Parqueo id _ _ _ _ _) = id;
getNombre (Parqueo _ nombre _ _ _ _) = nombre;
getDireccion (Parqueo _ _ direccion _ _ _) = direccion;
getProvincia (Parqueo _ _ _ provincia _ _) = provincia;
getUbicacionX (Parqueo _ _ _ _ ubicacionx _) = ubicacionx;
getUbicacionY (Parqueo _ _ _ _ _ ubicaciony) = ubicaciony;

-- ===============================================================================================================
-- ===============================================================================================================
type IdN = Int
type NombreN = String
type DireccionN = String
type ProvinciaN = String
type UbicacionXN = String
type UbicacionYN = String

data ParqueoN = ParqueoN IdN NombreN DireccionN ProvinciaN UbicacionXN UbicacionYN;
-- ===============================================================================================================
-- ======== Constructor de data parqueo y gets ===================================================================
crearParqueoN(elemento) = ParqueoN (read (elemento!!0) :: Int) (elemento!!1) (elemento!!2) (elemento!!3) (elemento!!4) (elemento!!5)
getIdN (ParqueoN id _ _ _ _ _) = id;
getNombreN (ParqueoN _ nombreN _ _ _ _) = nombreN;
getDireccionN (ParqueoN _ _ direccionN _ _ _) = direccionN;
getProvinciaN (ParqueoN _ _ _ provinciaN _ _) = provinciaN;
getUbicacionXN (ParqueoN _ _ _ _ ubicacionxN _) = ubicacionxN;
getUbicacionYN (ParqueoN _ _ _ _ _ ubicacionyN) = ubicacionyN;
-- ===============================================================================================================
-- ===============================================================================================================

-- ======== Convierte a String los datos de una data parqueo =====================================================
showParqueo :: Parqueo -> [Char]
showParqueo parqueo =
            let
                id = getId(parqueo)
                nombre = getNombre(parqueo)
                direccion = getDireccion(parqueo)
                provincia = getProvincia(parqueo)
                ubicacionx = getUbicacionX(parqueo)
                ubicaciony = getUbicacionY(parqueo)
            in 
                "Id: " ++ show id ++ "\n" ++
                "Nombre: " ++ nombre ++ "\n" ++
                "Direccion: " ++ direccion ++ "\n" ++
                "Provincia: " ++ provincia ++ "\n" ++
                "Ubicacion X: " ++ ubicacionx ++ "\n" ++
                "Ubicacion Y: " ++ ubicaciony ++ "\n"

-- ======== Muestra todas las data parqueo de una lista ===========================================
showParqueosN :: [ParqueoN] -> IO ()
showParqueosN [] = print("")
showParqueosN lista =
            do
                putStrLn(showParqueoN (head lista))
                showParqueosN (tail lista)

-- ======== Muestra todas las data parqueo de una lista ===========================================
showParqueos :: [Parqueo] -> IO ()
showParqueos [] = print("")
showParqueos lista =
            do
                putStrLn(showParqueo (head lista))
                showParqueos (tail lista)

-- =================================================================================================
-- ======== Convierte a String los datos de una data parqueo =====================================================
showParqueoN :: ParqueoN -> [Char]
showParqueoN parqueoN =
            let
                id = getIdN(parqueoN)
                nombre = getNombreN(parqueoN)
                direccion = getDireccionN(parqueoN)
                provincia = getProvinciaN(parqueoN)
                ubicacionx = getUbicacionXN(parqueoN)
                ubicaciony = getUbicacionYN(parqueoN)
            in 
                "Id: " ++ show id ++ "\n" ++
                "Nombre: " ++ nombre ++ "\n" ++
                "Direccion: " ++ direccion ++ "\n" ++
                "Provincia: " ++ provincia ++ "\n" ++
                "Ubicacion X: " ++ ubicacionx ++ "\n" ++
                "Ubicacion Y: " ++ ubicaciony ++ "\n"

-- ======== Split por comas ========================================================================
separarPorComas :: ([Char], [Char]) -> [[Char]]
separarPorComas (cadena, temp) =
    if cadena == "" then [temp] else
        if (head cadena) == (head ",") then
            [temp] ++ separarPorComas ((tail cadena), "")
            else
                separarPorComas((tail cadena), temp++[(head cadena)])

-- ======== Separa por comas una linea y lo convierte en data tipo parqueo =========================
separarElementosP :: [[Char]] -> [Parqueo]
separarElementosP lista =
    if lista == [] then
        []
    else
        [crearParqueo(separarPorComas((head lista), ""))] ++ separarElementosP (tail lista)

-- ==================================================================================================
separarElementosPNew :: [[Char]] -> [ParqueoN]
separarElementosPNew lista =
    if lista == [] then
        []
    else
        [crearParqueoN(separarPorComas((head lista), ""))] ++ separarElementosPNew (tail lista)

-- ======== Convierte en una lista un texto, parte el texto por cambios de lineas ===================
convierteAlineas :: String -> [String]
convierteAlineas texto = lines texto

-- ======== Lee todo el contenido de archivo, los convierte en lineas =================================
-- ======== luego parsea por comas las lineas y genera elementos parqueos =============================
leerArchivoP :: FilePath -> IO [Parqueo]
leerArchivoP archivo = do
    contenido <- readFile archivo
    let parqueos = separarElementosP (convierteAlineas contenido)
    return parqueos

-- ====================================================================================================
leerArchivoPNew :: FilePath -> IO [ParqueoN]
leerArchivoPNew archivo = do
    contenido <- readFile archivo
    let parqueos = separarElementosPNew (convierteAlineas contenido)
    return parqueos

-- ====================================================================================================
existeParqueo parqueos parqueosN = do
    existeParqueoAux (parqueos, parqueosN)

existeParqueoAux ([], parqueosN) = putStrLn ""
existeParqueoAux (parqueos, parqueosN) = do
    let primero = (head parqueos)
    let idNew = getId(primero)
    let resultado = existeIdAuxAux (parqueosN, idNew)
    if resultado == False then do
        let nuevoValor = showParqueoNew (head parqueos)
        escribirUltimaLinea "infoParqueo.txt" nuevoValor
        existeParqueoAux ((tail parqueos), parqueosN)
    else do
        existeParqueoAux ((tail parqueos), parqueosN)

existeIdAuxAux ([], idNew) = False 
existeIdAuxAux ((parqueoN:parqueosNRestantes), idNew) =
    let idNewN = getIdN parqueoN
    in
    if idNewN == idNew
        then True
        else existeIdAuxAux (parqueosNRestantes, idNew)

-- ====================================================================================================
showParqueoNew :: Parqueo -> String
showParqueoNew parqueo =
    let
        id = getId parqueo
        nombre = getNombre parqueo
        direccion = getDireccion parqueo
        provincia = getProvincia parqueo
        ubicacionx = getUbicacionX parqueo
        ubicaciony = getUbicacionY parqueo
    in
        show id ++ "," ++ nombre ++ "," ++ direccion ++ "," ++ provincia ++ "," ++ ubicacionx ++ "," ++ ubicaciony

escribirUltimaLinea :: FilePath -> String -> IO ()
escribirUltimaLinea archivo nuevoValor = do
    contenido <- readFile archivo
    let lineas = lines contenido
    case reverse lineas of
        [] -> writeFile archivo nuevoValor
        (ultimaLinea:lineasRestantes) -> do
            let nuevoContenido = contenido ++ "\n" ++ nuevoValor
            writeFile archivo nuevoContenido


