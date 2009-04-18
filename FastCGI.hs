module Happstack.Server.FastCGI where

import Network.FastCGI
import qualified Network.CGI as CGI
import Network.CGI.Monad (CGIRequest, cgiVars, cgiRequestBody, cgiGet)
import Network.CGI.Protocol (maybeRead)
import Happstack.Server hiding (processRequest)
import Happstack.Server.HTTP.Types (Request (..), Version (Version))
import qualified Happstack.Server as H
import qualified Data.Map as M
import Network.URI
import Control.Applicative
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.UTF8 as UBS
import Data.List (isPrefixOf)
import Data.Char (toLower)

numThreads :: Int
numThreads = 10

runServer :: (ToMessage b) => ServerPartT IO b -> IO ()
runServer = runFastCGIConcurrent numThreads . convert . processRequest

convert :: (Request -> IO Response) -> CGI CGIResult
convert f = do
  cgiReq <- cgiGet id
  q <- toHappstackRequest cgiReq
  resp <- liftIO (f q)
  toCGIResponse resp

toCGIResponse :: Response -> CGI CGIResult
toCGIResponse r = do
  -- todo rsflags
  --      rscode
  --      rsvalidator?
  mapM_ setHappstackHeader (M.elems $ rsHeaders r)
  outputFPS (rsBody r)

setHappstackHeader :: HeaderPair -> CGI ()
setHappstackHeader (HeaderPair k v) = do
  mapM_ (CGI.setHeader (UBS.toString k) . UBS.toString) v

toHappstackRequest :: CGIRequest -> CGI Request
toHappstackRequest rq = do
  i <- cgiInputs
  return $ Request {  rqMethod  = cgiMethod  rq
                   , rqPaths   = cgiPaths   rq
                   , rqUri	   = cgiUri     rq
                   , rqQuery   = cgiQuery   rq
                   , rqInputs  = i
                   , rqCookies = cgiCookies rq
                   , rqVersion = cgiVersion rq
                   , rqHeaders = cgiHeaders rq
                   , rqBody    = cgiBody    rq
                   , rqPeer    = cgiPeer    rq
                   }

cgiUri :: CGIRequest -> String
cgiUri = str "REQUEST_URI"

r ? k = M.lookup k $ cgiVars r
withDef x = maybe x id
str k v = withDef "" (v ? k)

split :: Char -> String -> [String]
split c cs = filter (not.null) $ worker [] cs
    where worker acc [] = [reverse acc]
          worker acc (c':cs) | c==c' = reverse acc:worker [] cs
          worker acc (c':cs) = worker (c':acc) cs

cgiMethod :: CGIRequest -> Method
cgiMethod x   = withDef GET $ (x ? "HTTP_METHOD") >>= maybeRead
cgiPaths      = split '/' . str "SCRIPT_NAME"
cgiQuery    x = '?':(str "QUERY_STRING" x)
cgiInputs     = getInputNames >>= mapM toHappstackInput
cgiCookies    = map cookieWithName . either (const []) id . parseCookies . str "HTTP_COOKIE"
cgiVersion    = parseProtocol . str "SERVER_PROTOCOL"
cgiHeaders :: CGIRequest -> Headers
cgiHeaders  = mkHeaders 
            . mapKeys   (drop (length httpStart))
            . filterKey (isPrefixOf httpStart) 
            . M.toList
            . cgiVars
cgiBody    = Body . cgiRequestBody
cgiPeer  _ = undefined -- TODO

httpStart = "HTTP_"

toHeaderPair k v = HeaderPair (UBS.fromString k) [UBS.fromString v]

cookieWithName x = (H.cookieName x, x)

mapKeys   f = map (\(k,v) -> (f k, v))
filterKey f = filter (f . fst)

parseProtocol "HTTP/0.9" = Version 0 9
parseProtocol "HTTP/1.0" = Version 1 0
parseProtocol "HTTP/1.1" = Version 1 1
parseProtocol _          = error "Invalid HTTP Version"

toHappstackInput :: String -> CGI (String, Input)
toHappstackInput k = do
  value <- withDef (BS.empty) <$> getInputFPS k
  filename <- getInputFilename k
  contentType <- withDef "" <$> getInputContentType k
  return $ (,) k $ Input {inputValue = value, inputFilename = filename, inputContentType = convertContentType $ parseContentType contentType }

convertContentType Nothing                        = error "No correct content-type"
convertContentType (Just (CGI.ContentType x y z)) = H.ContentType x y z

processRequest :: (ToMessage b, Monad m, Functor m) => ServerPartT m b -> Request -> m Response
processRequest hs req =  (runWebT $ runServerPartT hs req) >>= (return . (maybe standardNotFound id))
    where
    -- TODO
        standardNotFound = H.setHeader "Content-Type" "text/html" $ toResponse "NOT FOUND"


