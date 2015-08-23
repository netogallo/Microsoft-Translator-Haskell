{-# Language RecordWildCards, OverloadedStrings, DeriveDataTypeable #-}
module Language.Bing(
  BingLanguage(..),
  BingContext,
  BingError(..),
  ClientId,
  ClientSecret,
  checkToken,
  evalBing,
  execBing,
  getAccessToken,
  getAccessTokenEither,
  runBing,
  runExceptT,
  translate,
  translateM) where

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
import qualified Data.Text as T
import Network.URL (decString)
import Text.XML.Light.Input
import Text.XML.Light.Types
import Text.XML.Light.Proc
import Data.List (find)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.IO.Unsafe (unsafePerformIO)

type ClientId = ByteString

type ClientSecret = ByteString

data BingError = BingError ByteString
                 deriving (Typeable, Show)

-- | The languages available for Microsoft Translatorj
data BingLanguage = English
                  | German
                  | Norwegian
                  | Spanish

-- | Conversion function from Language to language code
toSym bl = case bl of
  English -> "en"
  German -> "de"
  Norwegian -> "no"
  Spanish -> "es"

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
  } deriving (Show,Typeable)

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

-- | Request a new access token from Azure using the specified client
-- id and client secret
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

-- | Check if the access token of the running BingAction is still
-- valid. If the token has expired, renews the token automatically
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

-- | Action that translates text inside a BingMonad context.
translateM :: Text -> BingLanguage -> BingLanguage -> BingMonad Text
translateM text from to = do
  let opts = N.defaults & N.param "from" .~ [toSym from :: Text]
             & N.param "to" .~ [toSym to]
             & N.param "contentType" .~ ["text/plain"]
             & N.param "category" .~ ["general"]
             & N.param "text" .~ [text]
  res <- getWithAuth opts translateUrl
  let trans = parseXML $ (TE.decodeUtf8 $ BLC.toStrict $ res ^. N.responseBody)
  case find (\n -> case n of
                Elem e -> "string" == (qName $ elName e)
                _ -> False) trans of
    Just (Elem e) -> return $ T.pack $ strContent e
    _ -> BM $ \_ -> throwE $ BingError $ pack $ show res

-- | Helper function that evaluates a BingMonad action. It simply
-- requests and access token and uses the token for evaluation.
evalBing :: ClientId -> ClientSecret -> BingMonad a -> IO (Either BingError a)
evalBing clientId clientSecret action = runExceptT $ do
  t <- getAccessToken clientId clientSecret
  runBing action t

getBingCtx :: BingMonad BingContext
getBingCtx = BM {runBing = \ctx -> return ctx}

execBing :: BingContext -> BingMonad a -> IO (Either BingError (a,BingContext))
execBing ctx action = runExceptT $ do
  flip runBing ctx $ do
    res <- action
    ctx <- getBingCtx
    return (res,ctx)

getAccessTokenEither :: ClientId -> ClientSecret -> IO (Either BingError BingContext)
getAccessTokenEither clientId clientSecret = runExceptT $ getAccessToken clientId clientSecret

-- | Toplevel wrapper that translates a text. It is only recommended if translation
-- is invoked less often than every 10 minutes since it always
-- requests a new access token.  For better performance use
-- translateM, runBing and getAccessToken
translate :: ClientId -> ClientSecret -> Text -> BingLanguage -> BingLanguage -> IO (Either BingError Text)
translate cid cs text from to = evalBing cid cs (translateM text from to)
