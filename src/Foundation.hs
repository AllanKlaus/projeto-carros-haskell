{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Database.Persist.Postgresql
import Yesod.Static
import Data.Text
import Data.Time

data Sitio = Sitio { connPool :: ConnectionPool,
                     getStatic :: Static }

staticFiles "."

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario
   nome Text
   pass Text
   deriving Show
   
Marcas
   marca Text
   deriving Show
   
Carros
   modelo Text
   valor Double
   qtd Int
   marcaid MarcasId
   deriving Show

Vendas
   userId UsuarioId
   carroId CarrosId
   qtde Int
   UniqueFornPeca userId carroId
|]

mkYesodData "Sitio" pRoutes

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just $ HomeR
    isAuthorized LoginR _ = return Authorized
    isAuthorized InserirMarcaR _ = isAdmin
    isAuthorized InserirCarroR _ = isAdmin
    isAuthorized InserirVendaR _ = isAdmin
    isAuthorized _ _ = isUser

isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "Somente Administrador"

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> Authorized
        Just _ -> Authorized

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage
