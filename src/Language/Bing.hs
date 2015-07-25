{-# Language RecordWildCards, OverloadedStrings, DeriveDataTypeable #-}
module Language.Bing where

import qualified Network.Wreq as N
import Network.Wreq.Types (Postable)
import Control.Lens
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack,unpack)
import Control.Monad.Catch
import Data.Typeable (Typeable)
import Control.Monad.IO.Class
import Network.HTTP.Client (HttpException)
import qualified Control.Exception as E
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Aeson
import Control.Monad (mzero)
import Control.Applicative ((<$>),(<*>))
import Data.Monoid
import Control.Applicative
import Data.DateTime
import Data.Text (Text)
import Network.URL (decString)
import Text.XML.Light.Input
import Text.XML.Light.Types
import Text.XML.Light.Proc
import Data.List (find)

type ClientId = ByteString

type ClientSecret = ByteString

data BingError = BingError ByteString
                 deriving (Typeable, Show)

data BingLanguage = English
                  | German

toSym bl = case bl of
  English -> "en"
  German -> "de"

data AccessToken = AccessToken {
  tokenType :: ByteString,
  token :: ByteString,
  expires :: Integer,
  scope :: ByteString
  } deriving Show

data BingContext = BCTX {
  accessToken :: AccessToken,
  inception :: DateTime,
  clientId :: ByteString,
  clientSecret :: ByteString
  } deriving Show

newtype BingMonad a = BM {runBing :: BingContext -> ExceptT BingError IO a}

instance Monad BingMonad where
  m >>= f = BM (\ctx' -> do
                   ctx <- checkToken ctx'
                   res <- runBing m ctx
                   runBing (f res) ctx)
            
  return a = BM $ \ctx -> return a

instance Functor BingMonad where
  fmap f bm = do
    v <- bm
    return $ f v

instance Applicative BingMonad where
  pure a = return a
  a <*> b = do
    a' <- a
    b' <- b
    return (a' b')

instance FromJSON AccessToken where
  parseJSON (Object v) = build <$>
                         v .: "token_type" <*>
                         v .: "access_token" <*>
                         ((v .: "expires_in") >>= getNum) <*>
                         v .: "scope"
    
    where
      getNum str = case decode (BLC.pack str) of
        Just n -> return n
        Nothing -> mzero
      build :: String -> String -> Integer -> String -> AccessToken
      build v1 v2 v3 v4 = AccessToken (pack v1) (pack v2) v3 (pack v4)
  parseJSON _ = mzero

instance Exception BingError

scopeArg = ("scope" :: ByteString)
        N.:= ("http://api.microsofttranslator.com" :: ByteString)

grantType = ("grant_type" :: ByteString)
            N.:= ("client_credentials" :: ByteString)

tokenAuthPage :: String
tokenAuthPage = "https://datamarket.accesscontrol.windows.net/v2/OAuth2-13"

translateUrl :: String
translateUrl = "http://api.microsofttranslator.com/v2/Http.svc/Translate"
-- translateUrl = "http://requestb.in/14zmco81"
 
translateArgs text from to = [
  ("text" N.:= (text :: ByteString)),
  ("from" N.:= (toSym from :: ByteString)),
  ("to" N.:= (toSym to :: ByteString))
  ]

bingAction :: IO (N.Response BL.ByteString) -> ExceptT BingError IO (N.Response BL.ByteString)
bingAction action = do
  res <- lift $ (E.try action :: IO (Either HttpException (N.Response BL.ByteString)))
  case res of
    Right res -> return res
    Left ex -> throwE $ BingError $ pack $ show ex

post url postable = bingAction (N.post url postable) 

postWith opts url postable = bingAction (N.postWith opts url postable)

getWithAuth opts' url = withContext $ \BCTX{..} -> do
  let opts = opts' & N.header "Authorization" .~ ["Bearer " <> token accessToken]
  bingAction (N.getWith opts url)

getAccessToken :: ByteString -> ByteString -> ExceptT BingError IO BingContext
getAccessToken clientId clientSecret = do
  req <- post tokenAuthPage  [
    "client_id" N.:= clientId,
    "client_secret" N.:= clientSecret,
    scopeArg,
    grantType
    ]
  r <- N.asJSON req
  let t = r ^. N.responseBody
  t' <- liftIO $ getCurrentTime
  return $ BCTX{
    accessToken = t,
    inception = t',
    clientId = clientId,
    clientSecret = clientSecret
    }
  
checkToken :: BingContext -> ExceptT BingError IO BingContext
checkToken ctx@BCTX{..} = do
  t <- liftIO $ getCurrentTime
  if diffSeconds t inception > expires accessToken - 100 then do
    BCTX{accessToken = tk} <- getAccessToken clientId clientSecret
    t' <- liftIO $ getCurrentTime
    return $ ctx{accessToken = tk, inception = t'}
  else
    return $ ctx

withContext = BM

-- translateM :: ByteString -> BingLanguage -> BingLanguage -> BingMonad (N.Response BLC.ByteString)
translateM text from to = do
  let opts = N.defaults & N.param "from" .~ [toSym from :: Text]
             & N.param "to" .~ [toSym to]
             & N.param "contentType" .~ ["text/plain"]
             & N.param "category" .~ ["general"]
             & N.param "text" .~ [text]
  res <- getWithAuth opts translateUrl
  let trans = parseXML $ res ^. N.responseBody
  case find (\n -> case n of
                Elem e -> "string" == (qName $ elName e)
                _ -> False) trans of
    Just (Elem e) -> return $ strContent e
    _ -> BM $ \_ -> throwE $ BingError $ pack $ show res

evalBing :: ByteString -> ByteString -> BingMonad a -> IO (Either BingError a)
evalBing clientId clientSecret action = runExceptT $ do
  t <- getAccessToken clientId clientSecret
  runBing action t
