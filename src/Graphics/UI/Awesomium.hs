module Graphics.UI.Awesomium 
    ( module Graphics.UI.Awesomium.Raw
    
    , Config (..)
    , defaultConfig
    , initialize
    , JSCallbackHandler
    , registerApiHandler
)where

import Foreign.Ptr
import Graphics.UI.Awesomium.Raw

data Config = Config 
    { pluginsEnabled            :: Bool
    , javascriptEnabled         :: Bool
    , databasesEnabled          :: Bool
    , packagePath               :: String
    , localePath                :: String
    , userDataPath              :: String
    , pluginPath                :: String
    , logPath                   :: String
    , logLevel                  :: LogLevel
    , singleProcess             :: Bool
    , childProcessPath          :: String
    , autoDetectEncodingEnabled :: Bool
    , acceptLanguageOverride    :: String
    , defaultCharsetOrerride    :: String
    , userAgentOverride         :: String
    , proxyServer               :: String
    , proxyConfigScript         :: String
    , authServerWhitelist       :: String
    , saveCacheAndCookies       :: Bool
    , maxCacheSize              :: Int
    , sameOriginPolicyEnabled   :: Bool
    , winMessagePumpEnabled     :: Bool
    , customCss                 :: String
} deriving (Show, Read, Eq)


defaultConfig :: Config
defaultConfig = Config
    { pluginsEnabled = False
    , javascriptEnabled         = True
    , databasesEnabled          = False
    , packagePath               = ""
    , localePath                = ""
    , userDataPath              = ""
    , pluginPath                = ""
    , logPath                   = ""
    , logLevel                  = Normal
    , singleProcess             = False
    , childProcessPath          = ""
    , autoDetectEncodingEnabled = True
    , acceptLanguageOverride    = ""
    , defaultCharsetOrerride    = ""
    , userAgentOverride         = ""
    , proxyServer               = ""
    , proxyConfigScript         = ""
    , authServerWhitelist       = ""
    , saveCacheAndCookies       = True
    , maxCacheSize              = 0
    , sameOriginPolicyEnabled   = True
    , winMessagePumpEnabled     = True
    , customCss                 = "" }

initialize :: Config -> IO ()
initialize c@(Config a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
                     a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23)
    | c == defaultConfig = awe_webcore_initialize_default
    | otherwise = awe_webcore_initialize a1 a2 a3 a4 a5 a6 a7 a8 a9
        a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23

type JSCallbackHandler = String -> String -> JSArray -> IO ()
defJSCallback :: (JSCallbackHandler) -> JSCallback
defJSCallback convcb wv ao afn arr = do
    o <- fromAweString ao
    fn <- fromAweString afn
    convcb o fn arr

-- | Creates API Handler for JS Callbacks. Returned FunPtr nedds to be
-- disposed with freeHaskellFunPtr when it's no longer needed
registerApiHandler :: WebView -> JSCallbackHandler
                   -> IO (FunPtr JSCallback)
registerApiHandler wv ah = do
    fp <- mkJSCallback (defJSCallback ah)
    awe_webview_set_callback_js_callback wv fp
    return fp



