{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
import Yesod.Static
 
pRoutes = [parseRoutes|
   /user UsuarioR GET POST
   /listar ListUserR GET
   /static StaticR Static getStatic
   /ima ImgR GET
   /login LoginR GET POST
   /logout ByeR GET
   
   / HomeR GET
   /home HomeLogadoR GET
   /inserirCarro InserirCarroR GET POST
   /listarCarro ListarCarroR GET
   /inserirMarca InserirMarcaR GET POST
   /listarMarca ListarMarcaR GET
   /carro/#CarrosId VerCarroR GET
   /vender InserirVendaR GET POST
   /listarVenda ListarVendaR GET
|]