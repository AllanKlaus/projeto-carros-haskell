{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Import
import Yesod
import Yesod.Static
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "form.hamlet")
     toWidget $(luciusFile "css.lucius")

widgetCss :: Widget
widgetCss = toWidget $(luciusFile "css.lucius")

formCarros :: Form Carros
formCarros = renderDivs $ Carros <$>
             areq textField "Modelo" Nothing <*>
             areq doubleField "Valor" Nothing <*>
             areq intField "Quantidade" Nothing <*>
             areq (selectField marcas) "Marca" Nothing

formMarcas :: Form Marcas
formMarcas = renderDivs $ Marcas <$>
             areq textField "Marca" Nothing

formVendas :: Form Vendas
formVendas = renderDivs $ Vendas <$>
             areq (selectField getUsuarios) "Usuario" Nothing <*>
             areq (selectField getCarros) "Carro" Nothing <*>
             areq intField "Qtde" Nothing

marcas = do
        entities <- runDB $ selectList [] [Asc MarcasMarca]
        optionsPairs $ Prelude.map (\cat -> (marcasMarca $ entityVal cat, entityKey cat)) entities

getUsuarios = do
        entities <- runDB $ selectList [] [Asc UsuarioNome]
        optionsPairs $ Prelude.map (\cat -> (usuarioNome $ entityVal cat, entityKey cat)) entities

getCarros = do
        entities <- runDB $ selectList [] [Asc CarrosModelo]
        optionsPairs $ Prelude.map (\cat -> (carrosModelo $ entityVal cat, entityKey cat)) entities

formUsu :: Form Usuario
formUsu = renderDivs $ Usuario <$>
    areq textField "Username" Nothing <*>
    areq passwordField "Pass" Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
    (wid,enc) <- generateFormPost formUsu
    defaultLayout $ widgetCss >> widgetHome >> widgetForm UsuarioR enc wid "Cadastro de Usuarios" "Cadastrar"

getImgR :: Handler Html
getImgR = defaultLayout [whamlet| 
    
|]

{-<img src=@{StaticR car_png}>-}

getLoginR :: Handler Html
getLoginR = do
     (wid,enc) <- generateFormPost formUsu
     defaultLayout $ widgetCss >> $(whamletFile "home.hamlet") >> widgetForm LoginR enc wid "" "Log in"

postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess usr -> do
            usuario <- runDB $ selectFirst [UsuarioNome ==. usuarioNome usr, UsuarioPass ==. usuarioPass usr ] []
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_ID" (usuarioNome usr)
                    redirect HomeR
                Nothing -> do
                    setMessage $ [shamlet| Invalid user |]
                    redirect LoginR 
        _ -> redirect LoginR

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Usuario inserido com sucesso! |]
            redirect UsuarioR
        _ -> redirect UsuarioR

getListUserR :: Handler Html
getListUserR = do
    listaU <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $ widgetCss >> widgetHome >> $(whamletFile "listUsuarios.hamlet")

getByeR :: Handler Html
getByeR = do
    deleteSession "_ID"
    defaultLayout (widgetCss >> widgetHome >> [whamlet| 
               <h2> Logout realizado com sucesso. 
           |])

widgetHome :: Widget
widgetHome = $(whamletFile "home.hamlet")

getHomeR :: Handler Html
getHomeR = do
     usr <- lookupSession "_ID"
     (wid,enc) <- generateFormPost formUsu
     defaultLayout $ widgetCss >> $(whamletFile "home.hamlet") >> widgetForm LoginR enc wid "Acesso Restrito" "Log in"


getHomeLogadoR :: Handler Html
getHomeLogadoR = do
     usr <- lookupSession "_ID"
     (wid,enc) <- generateFormPost formUsu
     defaultLayout $ widgetCss >> $(whamletFile "home.hamlet") >> [whamlet|
        $maybe m <- usr
            <h2> Welcome #{m}
     |]     

getInserirMarcaR :: Handler Html
getInserirMarcaR = do
         (widget, enctype) <- generateFormPost formMarcas
         defaultLayout $ widgetCss >> widgetHome >> (widgetForm InserirMarcaR  enctype widget "Cadastro de marca" "Cadastrar") 

postInserirMarcaR :: Handler Html
postInserirMarcaR = do
                ((result, _), _) <- runFormPost formMarcas
                case result of
                    FormSuccess marcas -> do
                       runDB $ insert marcas 
                       defaultLayout (widgetCss >> widgetHome >> [whamlet| 
                           <h2> Marca #{marcasMarca marcas} inserida com sucesso.
                       |])
                    _ -> redirect ListarMarcaR

getInserirCarroR :: Handler Html
getInserirCarroR = do
     (widget, enctype) <- generateFormPost formCarros
     defaultLayout $ widgetCss >> widgetHome >> (widgetForm InserirCarroR  enctype widget "Cadastro de carro" "Cadastrar")
         

postInserirCarroR :: Handler Html
postInserirCarroR = do
    ((result, _), _) <- runFormPost formCarros
    case result of
        FormSuccess carros -> do
           runDB $ insert carros 
           defaultLayout (widgetCss >> widgetHome >> [whamlet| 
               <h2> Carro #{carrosModelo carros} inserido com sucesso. 
           |])
        _ -> redirect ListarCarroR
     
getListarMarcaR :: Handler Html
getListarMarcaR = do
     marcas <- runDB $ selectList [] [Asc MarcasMarca]
     defaultLayout $ widgetCss >> widgetHome >> $(whamletFile "listMarcas.hamlet")

getListarCarroR :: Handler Html
getListarCarroR = do
     carros <- runDB $ selectList [] [Asc CarrosModelo]
     defaultLayout $ widgetCss >> widgetHome >> $(whamletFile "listCarros.hamlet")

getVerCarroR :: CarrosId -> Handler Html
getVerCarroR carid = do
             carro <- runDB $ get404 carid
             marca <- runDB $ get $ carrosMarcaid carro
             defaultLayout (widgetCss >> widgetHome >> $(whamletFile "getVerCarro.hamlet"))

fgetMarca :: MarcasId -> Handler Html
fgetMarca mid = do
             marca <- runDB $ get404 mid 
             defaultLayout (widgetCss >> widgetHome >> [whamlet| 
                 <p> Marca: #{marcasMarca marca}
             |])

getInserirVendaR :: Handler Html
getInserirVendaR = do
     (widget, enctype) <- generateFormPost formVendas
     defaultLayout $ widgetCss >> widgetHome >> (widgetForm InserirVendaR  enctype widget "Cadastro de venda" "Cadastrar")
         

postInserirVendaR :: Handler Html
postInserirVendaR = do
    ((result, _), _) <- runFormPost formVendas
    case result of
        FormSuccess vendas -> do
           runDB $ insert vendas 
           defaultLayout (widgetCss >> widgetHome >> [whamlet| 
               <h2> Venda realizada com sucesso. 
           |])
        _ -> redirect ListarVendaR

getListarVendaR :: Handler Html
getListarVendaR = do
     vendas <- runDB $ (rawSql "SELECT ??, ??, ?? \
                                   \FROM vendas INNER JOIN usuario \
                                   \ON vendas.user_id=usuario.id INNER JOIN carros \
                                   \ON vendas.carro_id=carros.id" [])::Handler [(Entity Vendas, Entity Usuario, Entity Carros)]
     defaultLayout $ widgetCss >> widgetHome >> $(whamletFile "listVendas.hamlet")


connStr = "dbname=d8m854lv9j1fsf host=ec2-54-227-248-123.compute-1.amazonaws.com user=jhkedfdupqyxrt password=qN_T00BC5zFXzB1ABXDByhiVbh port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Sitio pool s)