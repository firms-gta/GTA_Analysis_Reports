print("Initializing connection to the Global Tuna Atlas...")
dotenv::load_dot_env(file = file.path(here::here(),"tunaatlas_server.env"))
# Connection to the GTA database
con_GTA = dbConnect(drv = RPostgreSQL::PostgreSQL(), 
                    host = Sys.getenv("TUNA_ATLAS_DB_SERVER"), 
                    port = Sys.getenv("TUNA_ATLAS_DB_PORT"),
                    dbname = Sys.getenv("TUNA_ATLAS_DB_NAME"),
                    user = Sys.getenv("TUNA_ATLAS_DB_USER"),
                    password = Sys.getenv("TUNA_ATLAS_DB_PASSWORD")
                    )

print("GTA connection initialized!")
