module InfoUsuarios where
import Control.Monad (forM_)
import System.IO
-- ======== Registro de parqueo ==================================================================================
type Cedula = Int
type Nombre = String
data Usuario = Usuario Cedula Nombre;
-- ===============================================================================================================
-- ======== Constructor de data parqueo y gets ===================================================================
crearUsuario(elemento) = Usuario (read (elemento!!0) :: Int) (elemento!!1)
getCedula (Usuario cedula _) = cedula;
getNombre (Usuario _ nombre) = nombre;

-- ===============================================================================================================
-- ===============================================================================================================
type CedulaN = Int
type NombreN = String
data UsuarioN = UsuarioN CedulaN NombreN;
-- ===============================================================================================================
-- ======== Constructor de data parqueo y gets ===================================================================
crearUsuarioN(elemento) = UsuarioN (read (elemento!!0) :: Int) (elemento!!1)
getCedulaN (UsuarioN cedula _) = cedula;
getNombreN (UsuarioN _ nombre) = nombre;
-- ===============================================================================================================
-- ===============================================================================================================

-- ======== Convierte a String los datos de una data parqueo =====================================================
showUsuario :: Usuario -> [Char]
showUsuario usuario =
            let
                cedula = getCedula(usuario)
                nombre = getNombre(usuario)
            in 
                "Cedula: " ++ show cedula ++ "\n" ++
                "Nombre: " ++ nombre ++ "\n"

-- ======== Muestra todas las data usuarios de una lista ===========================================
showUsuarios:: [Usuario] -> IO ()
showUsuarios [] = print("")
showUsuarios lista =
            do
                putStrLn(showUsuario (head lista))
                showUsuarios (tail lista)

-- ======== Split por comas ========================================================================
separarPorComas :: ([Char], [Char]) -> [[Char]]
separarPorComas (cadena, temp) =
    if cadena == "" then [temp] else
        if (head cadena) == (head ",") then
            [temp] ++ separarPorComas ((tail cadena), "")
            else
                separarPorComas((tail cadena), temp++[(head cadena)])

-- ======== Separa por comas una linea y lo convierte en data tipo parqueo =========================
separarElementosU :: [[Char]] -> [Usuario]
separarElementosU lista =
    if lista == [] then
        []
    else
        [crearUsuario(separarPorComas((head lista), ""))] ++ separarElementosU (tail lista)

-- ==================================================================================================
separarElementosUNew :: [[Char]] -> [UsuarioN]
separarElementosUNew lista =
    if lista == [] then
        []
    else
        [crearUsuarioN(separarPorComas((head lista), ""))] ++ separarElementosUNew (tail lista)

-- ======== Convierte en una lista un texto, parte el texto por cambios de lineas ===================
convierteAlineas :: String -> [String]
convierteAlineas texto = lines texto

-- ======== Lee todo el contenido de archivo, los convierte en lineas =================================
-- ======== luego parsea por comas las lineas y genera elementos parqueos =============================
leerArchivoU :: FilePath -> IO [Usuario]
leerArchivoU archivo = do
    contenido <- readFile archivo
    let parqueos = separarElementosU (convierteAlineas contenido)
    return parqueos

-- ====================================================================================================
leerArchivoUNew :: FilePath -> IO [UsuarioN]
leerArchivoUNew archivo = do
    contenido <- readFile archivo
    let parqueos = separarElementosUNew (convierteAlineas contenido)
    return parqueos

-- ====================================================================================================
existeUsuario usuarios usuariosN = do
    existeUsuarioAux (usuarios, usuariosN)

existeUsuarioAux ([], usuariosN) = putStrLn ""
existeUsuarioAux (usuarios, usuariosN) = do
    let primero = (head usuarios)
    let cedulaNew1 = getCedula(primero)
    let resultado = existeUsuarioAuxAux (usuariosN, cedulaNew1)
    if resultado == False then do
        let nuevoValor = showUsuarioNew (head usuarios)
        escribirUltimaLinea "infoUsuarios.txt" nuevoValor
        existeUsuarioAux ((tail usuarios), usuariosN)
    else do
        existeUsuarioAux ((tail usuarios), usuariosN)

existeUsuarioAuxAux ([], cedulaNew1) = False 
existeUsuarioAuxAux ((usuariosN:usuariosNRestantes), cedulaNew1) =
    let cedulaNewN = getCedulaN usuariosN
    in
    if cedulaNewN == cedulaNew1
        then True
        else existeUsuarioAuxAux (usuariosNRestantes, cedulaNew1)

-- ====================================================================================================
showUsuarioNew :: Usuario -> String
showUsuarioNew usuario =
    let
        cedula = getCedula usuario
        nombre = getNombre usuario
    in
        show cedula ++ "," ++ nombre

escribirUltimaLinea :: FilePath -> String -> IO ()
escribirUltimaLinea archivo nuevoValor = do
    contenido <- readFile archivo
    let lineas = lines contenido
    case reverse lineas of
        [] -> writeFile archivo nuevoValor
        (ultimaLinea:lineasRestantes) -> do
            let nuevoContenido = contenido ++ "\n" ++ nuevoValor
            writeFile archivo nuevoContenido
