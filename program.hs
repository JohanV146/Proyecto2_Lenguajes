import System.IO
import InfoEmpresa

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
            let ruta = "info.txt"
            empresa1 <- leerArchivo ruta
            showEmpresas empresa1
            putStrLn "\n"
            menuRecursivo
        "2" -> do
            putStrLn "Has seleccionado la Opción 2."
            menuRecursivo  
        "3" -> do
            putStrLn "Has seleccionado la Opción 3."
            menuRecursivo 
        "4" -> do
            putStrLn "Has seleccionado la Opción 4."
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
