import System.IO
import InfoEmpresa
import InfoParqueos
import InfoUsuarios
import InfoBici

mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "Menú:"
    putStrLn "1. Informacion comercial"
    putStrLn "2. Cargar y Mostrar parqueos"
    putStrLn "3. Mostrar y Asignar bicicletas"
    putStrLn "4. Cargar usuarios"
    putStrLn "5. Estadisticas"
    putStrLn "6. Salir"
    putStr "Seleccione una opción: "
    hFlush stdout

menuRecursivo :: IO ()
menuRecursivo = do
    mostrarMenu
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "\n"
            let ruta = "InfoEmpresa.txt"
            empresa1 <- leerArchivo ruta
            showEmpresas empresa1
            putStrLn "\n"
            menuRecursivo
        "2" -> do
            putStrLn "\n"
            putStrLn "Indique la Ruta del archivo Parqueos: "
            ruta1 <- getLine
            let ruta2 = "infoParqueo.txt"
            parqueo1 <- leerArchivoP ruta1
            parqueo2 <- leerArchivoPNew ruta2
            existeParqueo parqueo1 parqueo2
            parqueo1 <- leerArchivoP ruta2
            showParqueos parqueo1
            putStrLn "\n"
            menuRecursivo  
        "3" -> do
            putStrLn "\n"
            let ruta = "infoBici.txt"
            bici <- leerArchivoB ruta
            putStrLn "Indique la Ruta del archivo ubicacion Bicicletas: "
            ruta2 <- getLine
            ubiBici <- leerArchivoBNew ruta2
            putStrLn "Se cargo con exito"
            putStrLn "\n"
            menuRecursivo 
        "4" -> do
            putStrLn "\n"
            putStrLn "Indique la Ruta del archivo Usuarios: "
            ruta1 <- getLine
            let ruta2 = "infoUsuarios.txt"
            usuario1 <- leerArchivoU ruta1
            usuario2 <- leerArchivoUNew ruta2
            existeUsuario usuario1 usuario2
            putStrLn "Se cargo con exito"
            putStrLn "\n"
            menuRecursivo
        "5" -> do
            putStrLn "Has seleccionado la Opción 5."
            menuRecursivo  
        "6" -> putStrLn "Saliendo del programa."
        _   -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            menuRecursivo 

main :: IO ()
main = do
    putStrLn "Bienvenido al programa."
    menuRecursivo
