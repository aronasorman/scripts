#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString         as B
import qualified Data.Map                as M
import           Data.Text               (Text)
import qualified Data.Text.IO            as TIO
import           GHC.Generics
import           Network.Wreq

main :: IO ()
main = uploadGist

data Gist =
  Gist { description :: Text
         , public    :: Bool
         , files     :: M.Map Text File
       } deriving (Show, Generic)

data File =
  File { content :: Text
       } deriving (Show, Generic)

instance FromJSON File
instance ToJSON File
instance FromJSON Gist
instance ToJSON Gist

initSpacemacsGist :: IO Gist
initSpacemacsGist = do
  spacemacsContents <- spacemacs
  return Gist { description = "Spacemacs file"
                , public = False
                , files = M.singleton ".spacemacs" spacemacsContents
              }
  where
    spacemacs = TIO.readFile "/home/aron/.spacemacs" >>= return . File

-- upload a file using POST
uploadGist :: IO ()
uploadGist = do
  token <- liftM oauth2Token (B.readFile "github.token")
  tokenOpts <- return $ defaults  & auth ?~ token
  gistPayload <- liftM encode initSpacemacsGist
  _ <- postWith tokenOpts "https://api.github.com/gists/b22eaf3fb7fc5acd19d0517620d5a098" gistPayload
  return ()
