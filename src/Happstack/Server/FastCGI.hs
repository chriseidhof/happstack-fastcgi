{- | 
Running Happstack applications using FastCGI
    
You need to keep a couple things in mind when configuring a FastCGI Happstack application, especially when using Happstack-state.

There are several ways to let Apache + FastCGI handle your application.

[Dynamic] This is the easy way.  You don't have to configure your server, but can just execute the scripts.  FastCGI will spawn instances of your application if needed and kill them if they're not needed anymore.  /This might break working with Happstack-state!/

[Static] You explicitly need to configure your script in your host config.  By default it will only start one process, on server startup.  If you want to work with Happstack-state, this is the preferable way, although we have not exhaustively tested that it won't break.
| -} 
module Happstack.Server.FastCGI 
    ( module Network.FastCGI
    , serverPartToCGI
    ) 
    where

import Control.Applicative
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Happstack.Server
import Happstack.Server.HTTP.Types (Request (..), Version (Version))
import Network.CGI.Monad (CGIRequest, cgiVars, cgiRequestBody, cgiGet)
import Network.CGI.Protocol (maybeRead)
import Network.FastCGI
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.UTF8 as UBS
import qualified Data.Map as M
import qualified Happstack.Server as H
import qualified Network.CGI as CGI


-- | Converts a Happstack ServerPartT to a CGI handling function.
serverPartToCGI :: (ToMessage b) => ServerPartT IO b -> CGI CGIResult
serverPartToCGI = convert . processRequest


convert :: (Request -> IO Response) -> CGI CGIResult
convert f = cgiGet id 
        >>= toHappstackRequest 
        >>= liftIO . f 
        >>= toCGIResponse

-- Converts the Happstack response into a CGIResult, setting the statuscode, headers and body.
toCGIResponse :: Response -> CGI CGIResult
toCGIResponse r = do
  r'    <- liftIO (runValidator return r)
  let c  = rsCode r'
  CGI.setStatus c (responseMessage c)
  mapM_ setHappstackHeader (M.elems $ rsHeaders r')
  outputFPS (rsBody r')

-- | Sets all the headers coming from Happstack
setHappstackHeader :: HeaderPair -> CGI ()
setHappstackHeader (HeaderPair k v) = 
  mapM_ (CGI.setHeader (UBS.toString k) . UBS.toString) v

-- | Converts one request into another
toHappstackRequest :: CGIRequest -> CGI Request
toHappstackRequest rq = do
  i <- cgiInputs
  return $ Request { rqMethod  = cgiMethod  rq
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

-- | Lookup a variable in the cgiVars
(?)   :: CGIRequest -> String -> Maybe String
r ? k = M.lookup k $ cgiVars r

-- | Like fromJust, but with a default value in case of Nothing.
withDef x = maybe x id

-- | Look up a String in the cgiVars, returning the empty string if the key is not present
str k v   = withDef "" (v ? k)

cgiUri :: CGIRequest -> String
cgiUri = str "REQUEST_URI"

cgiMethod :: CGIRequest -> Method
cgiMethod x = withDef GET $ (x ? "HTTP_METHOD") >>= maybeRead

cgiPaths :: CGIRequest -> [String]
cgiPaths = split '/' . str "PATH_INFO"

cgiQuery :: CGIRequest -> String
cgiQuery    x = '?':(str "QUERY_STRING" x)

cgiInputs :: CGI [(String, Input)]
cgiInputs = getInputNames >>= mapM toHappstackInput

cgiCookies :: CGIRequest -> [(String, H.Cookie)]
cgiCookies    = map cookieWithName . either (const []) id . parseCookies . str "HTTP_COOKIE"

cgiVersion :: CGIRequest -> Version
cgiVersion    = parseProtocol . str "SERVER_PROTOCOL"

cgiHeaders :: CGIRequest -> Headers
cgiHeaders  = mkHeaders 
            . mapKeys   (replace '_' '-' . drop (length httpPrefix))
            . filterKey (isPrefixOf httpPrefix) 
            . M.toList
            . cgiVars

cgiBody :: CGIRequest -> RqBody
cgiBody    = Body . cgiRequestBody

cgiPeer :: CGIRequest -> (String, Int)
cgiPeer  r = (str "REMOTE_ADDR" r, withDef 0 (r ? "REMOTE_PORT" >>= maybeRead)) -- TODO


-- | Replace x by y in a map
replace :: (Eq a) => a -> a -> [a] -> [a]
replace x y = map (\v -> if v == x then y else v)

httpPrefix = "HTTP_"

toHeaderPair :: String -> String -> HeaderPair
toHeaderPair k v = HeaderPair (UBS.fromString k) [UBS.fromString v]

cookieWithName :: H.Cookie -> (String, H.Cookie)
cookieWithName x = (H.cookieName x, x)

mapKeys   f = map (\(k,v) -> (f k, v))
filterKey f = filter (f . fst)


-- | Parse the HTTP protocol
parseProtocol :: String -> Version
parseProtocol "HTTP/0.9" = Version 0 9
parseProtocol "HTTP/1.0" = Version 1 0
parseProtocol "HTTP/1.1" = Version 1 1
parseProtocol _          = error "Invalid HTTP Version"

-- | Gives an input key/value given an input key
toHappstackInput :: String -> CGI (String, Input)
toHappstackInput k = do
  filename    <- getInputFilename k
  value       <- withDef (BS.empty) <$> getInputFPS k
  contentType <- withDef ""         <$> getInputContentType k
  return (k,  Input { inputValue       = value
                    , inputFilename    = filename
                    , inputContentType = convertContentType $ parseContentType contentType 
                    })


-- | Converts one Content type into the other
convertContentType :: Maybe CGI.ContentType -> H.ContentType
convertContentType (Just (CGI.ContentType x y z)) = H.ContentType x y z
convertContentType Nothing                        = error "No correct content-type"

-- | Transforms a ServerPartT into a function. This is a copy of simpleHTTP'
processRequest :: (ToMessage b, Monad m, Functor m) => ServerPartT m b -> Request -> m Response
processRequest hs req =  (runWebT $ runServerPartT hs req) >>= (return . (maybe standardNotFound id))
    where
        standardNotFound = H.setHeader "Content-Type" "text/html" $ toResponse "Not found"


--------------------------------------------------
-- Copied straight from Lemmih's old happs-fastcgi
--------------------------------------------------

responseMessage :: Int -> [Char]
responseMessage 100 = "100 Continue"
responseMessage 101 = "101 Switching Protocols"
responseMessage 200 = "200 OK"
responseMessage 201 = "201 Created"
responseMessage 202 = "202 Accepted"
responseMessage 203 = "203 Non-Authoritative Information"
responseMessage 204 = "204 No Content"
responseMessage 205 = "205 Reset Content"
responseMessage 206 = "206 Partial Content"
responseMessage 300 = "300 Multiple Choices"
responseMessage 301 = "301 Moved Permanently"
responseMessage 302 = "302 Found"
responseMessage 303 = "303 See Other"
responseMessage 304 = "304 Not Modified"
responseMessage 305 = "305 Use Proxy"
responseMessage 307 = "307 Temporary Redirect"
responseMessage 400 = "400 Bad Request"
responseMessage 401 = "401 Unauthorized"
responseMessage 402 = "402 Payment Required"
responseMessage 403 = "403 Forbidden"
responseMessage 404 = "404 Not Found"
responseMessage 405 = "405 Method Not Allowed"
responseMessage 406 = "406 Not Acceptable"
responseMessage 407 = "407 Proxy Authentication Required"
responseMessage 408 = "408 Request Time-out"
responseMessage 409 = "409 Conflict"
responseMessage 410 = "410 Gone"
responseMessage 411 = "411 Length Required"
responseMessage 412 = "412 Precondition Failed"
responseMessage 413 = "413 Request Entity Too Large"
responseMessage 414 = "414 Request-URI Too Large"
responseMessage 415 = "415 Unsupported Media Type"
responseMessage 416 = "416 Requested range not satisfiable"
responseMessage 417 = "417 Expectation Failed"
responseMessage 500 = "500 Internal Server Error"
responseMessage 501 = "501 Not Implemented"
responseMessage 502 = "502 Bad Gateway"
responseMessage 503 = "503 Service Unavailable"
responseMessage 504 = "504 Gateway Time-out"
responseMessage 505 = "505 HTTP Version not supported"
responseMessage x   = (show x ++ "\r\n")

-- | Splits a list by character, the resulting lists don't have the character in them.
split :: Char -> String -> [String]
split c cs = filter (not.null) $ worker [] cs
    where worker acc [] = [reverse acc]
          worker acc (c':cs) | c==c' = reverse acc:worker [] cs
          worker acc (c':cs) = worker (c':acc) cs
