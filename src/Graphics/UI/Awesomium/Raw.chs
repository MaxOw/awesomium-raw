{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.UI.Awesomium.Raw where

import Data.Char (chr, ord)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Control.Monad ((<=<))
import Control.Applicative
import Control.Exception (bracket)

import C2HS

{#context prefix = "awe"#}

#define bool uint
#include "awesomium_capi.h"

type WChar16 = {#type wchar16#}
type Int64 = {#type int64#}

-- | WebView instance
data DWebView
{#pointer *webview as WebView -> DWebView #}
-- | JSValue instance
data DJSValue
{#pointer *jsvalue as JSValue -> DJSValue #}
-- | JSArray instance
data DJSArray
{#pointer *jsarray as JSArray -> DJSArray #}
-- | JSObject instance
data DJSObject
{#pointer *jsobject as JSObject -> DJSObject #}
-- | RenderBuffer instance, owned by the WebView
data DRenderBuffer
{#pointer *renderbuffer as RenderBuffer -> DRenderBuffer #}
-- | HeaderDefinition instance
data DHeaderDefinition
{#pointer *header_definition as HeaderDefinition -> DHeaderDefinition #}
-- | ResourceResponse instance
data DResourceResponse
{#pointer *resource_response as ResourceResponse -> DResourceResponse #}
-- | ResourceRequest instance
data DResourceRequest
{#pointer *resource_request as ResourceRequest -> DResourceRequest #}
-- | UploadElement instance
data DUploadElement
{#pointer *upload_element as UploadElement -> DUploadElement #}
-- | String instance
data DAweString
{#pointer *awe_string as AweString -> DAweString #}
-- | HistoryQueryResult instance
data DHistoryQueryResult
{#pointer *history_query_result as HistoryQueryResult -> DHistoryQueryResult #}
-- | HistoryEntry instance
data DHistoryEntry
{#pointer *history_entry as HistoryEntry -> DHistoryEntry #}

{#enum loglevel as LogLevel {underscoreToCase} with prefix = "AWE_LL_" deriving (Show, Read, Eq)#}
{#enum mousebutton as MouseButton {underscoreToCase}#}
{#enum url_filtering_mode as UrlFilteringMode {underscoreToCase}#}
{#enum webkey_type as WebkeyType {underscoreToCase}#}
{#enum webkey_modifiers as WebkeyModifiers {underscoreToCase}#}
{#enum cursor_type as CursorType {underscoreToCase}#}
{#enum ime_state as ImeState {underscoreToCase}#}
{#enum media_type as MediaType {underscoreToCase}#}
{#enum _awe_media_state as MediaState {underscoreToCase} with prefix = "AWE_" #}
{#enum _awe_can_edit_flags as CanEditFlags {underscoreToCase} with prefix = "AWE_" #}
{#enum _awe_dialog_flags as DialogFlags {underscoreToCase} with prefix = "AWE_" #}

{-
These are flags, remember!
    * WebkeyModifiers
    * MediaState
    * CanEditFlags
    * DialogFlags
-}

data WebkeyboardEvent = WebkeyboardEvent
    { wkeType           :: WebkeyType
    , wkeModifiers      :: Int
    , wkeVirtualKeyCode :: Int
    , wkeNativeKeyCode  :: Int
    , wkeText           :: Char
    , wkeUnmodifiedText :: Char
    , wkeIsSystemKey    :: Bool }

instance Storable WebkeyboardEvent where
    alignment _ = {#alignof awe_webkeyboardevent#}
    sizeOf _ = {#sizeof awe_webkeyboardevent#}
    peek p =
        let {fromWkeText = return . chr . fromIntegral . head <=< peekArray 4} in
        WebkeyboardEvent
        <$> fmap cToEnum      ({#get awe_webkeyboardevent.type             #} p)
        <*> fmap fromIntegral ({#get awe_webkeyboardevent.modifiers        #} p)
        <*> fmap fromIntegral ({#get awe_webkeyboardevent.virtual_key_code #} p)
        <*> fmap fromIntegral ({#get awe_webkeyboardevent.native_key_code  #} p)
        <*> (fromWkeText =<<  ({#get awe_webkeyboardevent.text             #} p))
        <*> (fromWkeText =<<  ({#get awe_webkeyboardevent.unmodified_text  #} p))
        <*> fmap toBool       ({#get awe_webkeyboardevent.is_system_key    #} p)
    poke p r = do
        let toWkeText l = let {l' = fromIntegral . ord $ l} in newArray [l',0,0,0] -- memory leaks here, probably
        ({#set awe_webkeyboardevent.type             #}) p (cFromEnum     $ wkeType           r)
        ({#set awe_webkeyboardevent.modifiers        #}) p (fromIntegral  $ wkeModifiers      r)
        ({#set awe_webkeyboardevent.virtual_key_code #}) p (fromIntegral  $ wkeVirtualKeyCode r)
        ({#set awe_webkeyboardevent.native_key_code  #}) p (fromIntegral  $ wkeNativeKeyCode  r)
        ({#set awe_webkeyboardevent.text             #}) p =<< (toWkeText $ wkeText           r)
        ({#set awe_webkeyboardevent.unmodified_text  #}) p =<< (toWkeText $ wkeUnmodifiedText r)
        ({#set awe_webkeyboardevent.is_system_key    #}) p (fromBool      $ wkeIsSystemKey    r)
        
data Rect = Rect
    { rectX      :: Int
    , rectY      :: Int
    , rectWidth  :: Int
    , rectHeight :: Int }
    deriving (Eq, Show)

instance Storable Rect where
    alignment _ = {#alignof awe_rect#}
    sizeOf _ = {#sizeof awe_rect#}
    peek p = let f = fmap fromIntegral in Rect
        <$> f ({#get awe_rect.x      #} p)
        <*> f ({#get awe_rect.y      #} p)
        <*> f ({#get awe_rect.width  #} p)
        <*> f ({#get awe_rect.height #} p)
    poke p r = do
        ({#set awe_rect.x      #}) p (fromIntegral $ rectX      r)
        ({#set awe_rect.y      #}) p (fromIntegral $ rectY      r)
        ({#set awe_rect.width  #}) p (fromIntegral $ rectWidth  r)
        ({#set awe_rect.height #}) p (fromIntegral $ rectHeight r)

{-
#ifdef _WIN32
{#fun awe_is_child_process { HINSTANCE hInstance } -> `Bool' #}
{#fun awe_child_process_main { HINSTANCE hInstance } -> `Int' #}
#else
{#fun awe_is_child_process { `Int', char** argv } -> `Bool' #}
{#fun awe_child_process_main { `Int', char** argv } -> `Int' #}
#endif
-}

{-----------------------
 - AweString Functions -
 -----------------------}

{#fun awe_string_empty { } -> `AweString' id #}
-- {#fun awe_string_create_orom_ascii { `String'& } -> `AweString' id #}
-- {#fun awe_string_create_from_wide { `String'& } -> `AweString' id #}
{#fun awe_string_create_from_utf8 { `String'& } -> `AweString' id #}
-- {#fun awe_string_create_from_utf16 { `String'& } -> `AweString' id #}
{#fun awe_string_destroy { id `AweString' } -> `()' #}
{#fun awe_string_get_length { id `AweString' } -> `Int' fromIntegral #}
-- {#fun awe_string_get_utf16 { id `AweString' } -> `String' #}
-- {#fun awe_string_to_wide { id `AweString' , `String'& } -> `Int' #}

foreign import ccall "Graphics/UI/Awesomium/Raw.chs.h awe_string_to_utf8"
    awe_string_to_utf8'_ :: AweString -> Ptr CChar -> CULong -> IO CInt

awe_string_to_utf8 :: AweString -> IO (String)
awe_string_to_utf8 a1 = do
    len <- awe_string_get_length a1
    allocaBytes len $ \buf -> do
        awe_string_to_utf8'_ a1 buf (cIntConv len)
        peekCStringLen (buf, (cIntConv len))

-- | Use with c functions that return const awe_string*
fromAweString :: AweString -> IO (String)
fromAweString = awe_string_to_utf8

-- | Use with c functions that return awe_string* that should be destroyed afterwards.
fromAweStringDestroy :: AweString -> IO (String)
fromAweStringDestroy as = do
    res <- awe_string_to_utf8 as
    awe_string_destroy as
    return res

withAweString :: String -> (AweString -> IO b) -> IO b
withAweString str =
    bracket (awe_string_create_from_utf8 str) awe_string_destroy

{-----------------------
 - Web Core Functions  -
 -----------------------}

{#fun awe_webcore_initialize { `Bool', `Bool', `Bool', withAweString* `String', withAweString* `String', withAweString* `String', withAweString* `String', withAweString* `String', cFromEnum `LogLevel', `Bool', withAweString* `String', `Bool', withAweString* `String', withAweString* `String', withAweString* `String', withAweString* `String', withAweString* `String', withAweString* `String', `Bool', `Int', `Bool', `Bool', withAweString* `String' } -> `()' #}
{#fun awe_webcore_initialize_default { } -> `()' #}
{#fun awe_webcore_shutdown { } -> `()' #}
{#fun awe_webcore_set_base_directory { withAweString* `String' } -> `()' #}
{#fun awe_webcore_create_webview { `Int', `Int', `Bool' } -> `WebView' id #}
{#fun awe_webcore_set_custom_response_page { `Int', withAweString* `String' } -> `()' #}
{#fun awe_webcore_update { } -> `()' #}
{#fun awe_webcore_get_base_directory { } -> `String' fromAweString* #}
{#fun awe_webcore_are_plugins_enabled { } -> `Bool' #}
{#fun awe_webcore_clear_cache { } -> `()' #}
{#fun awe_webcore_clear_cookies { } -> `()' #}
{#fun awe_webcore_set_cookie { withAweString* `String', withAweString* `String', `Bool', `Bool' } -> `()' #}
{#fun awe_webcore_get_cookies { withAweString* `String', `Bool' } -> `String' fromAweString* #}
{#fun awe_webcore_delete_cookie { withAweString* `String', withAweString* `String' } -> `()' #}
{#fun awe_webcore_set_suppress_printer_dialog { `Bool' } -> `()' #}
{#fun awe_webcore_query_history { withAweString* `String', `Int', `Int' } -> `HistoryQueryResult' id #}

{-----------------------
 - Web View Functions  -
 -----------------------}

{#fun awe_webview_destroy { id `WebView' } -> `()' #}
{#fun awe_webview_load_url { id `WebView', withAweString* `String', withAweString* `String', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun awe_webview_load_html { id `WebView', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun awe_webview_load_file { id `WebView', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun awe_webview_get_url { id `WebView' } -> `String' fromAweStringDestroy* #}
{#fun awe_webview_go_to_history_offset { id `WebView', `Int' } -> `()' #}
{#fun awe_webview_get_history_back_count { id `WebView' } -> `Int' #}
{#fun awe_webview_get_history_forward_count { id `WebView' } -> `Int' #}
{#fun awe_webview_stop { id `WebView' } -> `()' #}
{#fun awe_webview_reload { id `WebView' } -> `()' #}
{#fun awe_webview_execute_javascript { id `WebView', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun awe_webview_execute_javascript_with_result { id `WebView', withAweString* `String', withAweString* `String', `Int' } -> `JSValue' id #}
{#fun awe_webview_call_javascript_function { id `WebView', withAweString* `String', withAweString* `String', id `JSArray', withAweString* `String' } -> `()' #}
{#fun awe_webview_create_object { id `WebView', withAweString* `String' } -> `()' #}
{#fun awe_webview_destroy_object { id `WebView', withAweString* `String' } -> `()' #}
{#fun awe_webview_set_object_property { id `WebView', withAweString* `String', withAweString* `String', id `JSValue' } -> `()' #}
{#fun awe_webview_set_object_callback { id `WebView', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun awe_webview_is_loading_page { id `WebView' } -> `Bool' #}
{#fun awe_webview_is_dirty { id `WebView' } -> `Bool' #}

foreign import ccall safe "Graphics/UI/Awesomium/Raw.chs.h awe_webview_get_dirty_bounds"
  awe_webview_get_dirty_bounds'_ :: WebView -> IO (Ptr Rect)
awe_webview_get_dirty_bounds :: WebView -> IO (Rect)
awe_webview_get_dirty_bounds = peek <=< awe_webview_get_dirty_bounds'_

{#fun awe_webview_render { id `WebView' } -> `RenderBuffer' id #}
{#fun awe_webview_pause_rendering { id `WebView' } -> `()' #}
{#fun awe_webview_resume_rendering { id `WebView' } -> `()' #}
{#fun awe_webview_inject_mouse_move { id `WebView', `Int', `Int' } -> `()' #}
{#fun awe_webview_inject_mouse_down { id `WebView', cFromEnum `MouseButton' } -> `()' #}
{#fun awe_webview_inject_mouse_up { id `WebView', cFromEnum `MouseButton' } -> `()' #}
{#fun awe_webview_inject_mouse_wheel { id `WebView', `Int', `Int' } -> `()' #}

foreign import ccall safe "Graphics/UI/Awesomium/Raw.chs.h awe_webview_inject_keyboard_event"
  awe_webview_inject_keyboard_event'_ :: WebView -> Ptr WebkeyboardEvent -> IO ()
awe_webview_inject_keyboard_event :: WebView -> WebkeyboardEvent -> IO ()
awe_webview_inject_keyboard_event wv e = with e $ \e' -> 
    awe_webview_inject_keyboard_event'_ wv e'

#ifdef _WIN32
-- {#fun awe_webview_inject_keyboard_event_win { id `WebView', UINT msg, WPARAM wparam, LPARAM lparam } -> `()' #}
#endif

{#fun awe_webview_cut { id `WebView' } -> `()' #}
{#fun awe_webview_copy { id `WebView' } -> `()' #}
{#fun awe_webview_paste { id `WebView' } -> `()' #}
{#fun awe_webview_select_all { id `WebView' } -> `()' #}
{#fun awe_webview_copy_image_at { id `WebView', `Int', `Int' } -> `()' #}
{#fun awe_webview_set_zoom { id `WebView', `Int' } -> `()' #}
{#fun awe_webview_reset_zoom { id `WebView' } -> `()' #}
{#fun awe_webview_get_zoom { id `WebView' } -> `Int' #}
{#fun awe_webview_get_zoom_for_host { id `WebView', withAweString* `String' } -> `Int' #}
{#fun awe_webview_resize { id `WebView', `Int', `Int', `Bool', `Int' } -> `Bool' #}
{#fun awe_webview_is_resizing { id `WebView' } -> `Bool' #}
{#fun awe_webview_unfocus { id `WebView' } -> `()' #}
{#fun awe_webview_focus { id `WebView' } -> `()' #}
{#fun awe_webview_set_transparent { id `WebView', `Bool' } -> `()' #}
{#fun awe_webview_is_transparent { id `WebView' } -> `Bool' #}
{#fun awe_webview_set_url_filtering_mode { id `WebView', cFromEnum `UrlFilteringMode' } -> `()' #}
{#fun awe_webview_add_url_filter { id `WebView', withAweString* `String' } -> `()' #}
{#fun awe_webview_clear_all_url_filters { id `WebView' } -> `()' #}

foreign import ccall safe "Graphics/UI/Awesomium/Raw.chs.h awe_webview_set_header_definition"
  awe_webview_set_header_definition'_ :: ((WebView) -> ((AweString) -> (CULong -> ((Ptr (AweString)) -> ((Ptr (AweString)) -> (IO ()))))))
awe_webview_set_header_definition :: WebView -> String -> [(String, String)] -> IO ()
awe_webview_set_header_definition wv n l =
    withAweString n $ \n' -> 
    let (fns, fvs) = unzip l in
    withMany withAweString fns $ \ns ->
    withArray ns $ \ns' ->
    withMany withAweString fvs $ \vs ->
    withArray vs $ \vs' ->
    let len' = fromIntegral . length $ l in
    awe_webview_set_header_definition'_ wv n' len' ns' vs'

{#fun awe_webview_add_header_rewrite_rule { id `WebView', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun awe_webview_remove_header_rewrite_rule { id `WebView', withAweString* `String' } -> `()' #}
{#fun awe_webview_remove_header_rewrite_rules_by_definition_name { id `WebView', withAweString* `String' } -> `()' #}
{#fun awe_webview_choose_file { id `WebView', withAweString* `String' } -> `()' #}
{#fun awe_webview_print { id `WebView' } -> `()' #}
{#fun awe_webview_request_scroll_data { id `WebView', withAweString* `String' } -> `()' #}
{#fun awe_webview_find { id `WebView', `Int', withAweString* `String', `Bool', `Bool', `Bool' } -> `()' #}
{#fun awe_webview_stop_find { id `WebView', `Bool' } -> `()' #}
{#fun awe_webview_translate_page { id `WebView', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun awe_webview_activate_ime { id `WebView', `Bool' } -> `()' #}
{#fun awe_webview_set_ime_composition { id `WebView', withAweString* `String', `Int', `Int', `Int' } -> `()' #}
{#fun awe_webview_confirm_ime_composition { id `WebView', withAweString* `String' } -> `()' #}
{#fun awe_webview_cancel_ime_composition { id `WebView' } -> `()' #}
{#fun awe_webview_login { id `WebView', `Int', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun awe_webview_cancel_login { id `WebView', `Int' } -> `()' #}
{#fun awe_webview_close_javascript_dialog { id `WebView', `Int', `Bool', withAweString* `String' } -> `()' #}

type BeginNavigationCallback = WebView -> AweString -> AweString -> IO()
foreign import ccall "wrapper" mkBeginNavigationCallback :: BeginNavigationCallback -> IO (FunPtr BeginNavigationCallback)
{#fun awe_webview_set_callback_begin_navigation { id `WebView', id `FunPtr BeginNavigationCallback' } -> `()' #}

type BeginLoadingCallback = WebView -> AweString -> AweString -> CInt -> AweString -> IO()
foreign import ccall "wrapper" mkBeginLoadingCallback :: BeginLoadingCallback -> IO (FunPtr BeginLoadingCallback)
{#fun awe_webview_set_callback_begin_loading { id `WebView', id `FunPtr BeginLoadingCallback' } -> `()' #}

type FinishLoadingCallback = WebView -> IO()
foreign import ccall "wrapper" mkFinishLoadingCallback :: FinishLoadingCallback -> IO (FunPtr FinishLoadingCallback)
{#fun awe_webview_set_callback_finish_loading { id `WebView', id `FunPtr FinishLoadingCallback' } -> `()' #}

type JSCallback = WebView -> AweString -> AweString -> JSArray -> IO()
foreign import ccall "wrapper" mkJSCallback :: JSCallback -> IO (FunPtr JSCallback)
{#fun awe_webview_set_callback_js_callback { id `WebView', id `FunPtr JSCallback' } -> `()' #}

type ReceiveTitleCallback = WebView -> AweString -> AweString -> IO()
foreign import ccall "wrapper" mkReceiveTitleCallback :: ReceiveTitleCallback -> IO (FunPtr ReceiveTitleCallback)
{#fun awe_webview_set_callback_receive_title { id `WebView', id `FunPtr ReceiveTitleCallback' } -> `()' #}

type ChangeTooltipCallback = WebView -> AweString -> IO()
foreign import ccall "wrapper" mkChangeTooltipCallback :: ChangeTooltipCallback -> IO (FunPtr ChangeTooltipCallback)
{#fun awe_webview_set_callback_change_tooltip { id `WebView', id `FunPtr ChangeTooltipCallback' } -> `()' #}

type ChangeCursorCallback = WebView -> CInt {-CursorType-} -> IO()
foreign import ccall "wrapper" mkChangeCursorCallback :: ChangeCursorCallback -> IO (FunPtr ChangeCursorCallback)
{#fun awe_webview_set_callback_change_cursor { id `WebView', id `FunPtr ChangeCursorCallback' } -> `()' #}

type ChangeKeyboardFocusCallback = WebView -> CUInt {-Bool-} -> IO()
foreign import ccall "wrapper" mkChangeKeyboardFocusCallback :: ChangeKeyboardFocusCallback -> IO (FunPtr ChangeKeyboardFocusCallback)
{#fun awe_webview_set_callback_change_keyboard_focus { id `WebView', id `FunPtr ChangeKeyboardFocusCallback' } -> `()' #}

type ChangeTargetUrlCallback = WebView -> AweString -> IO()
foreign import ccall "wrapper" mkChangeTargetUrlCallback :: ChangeTargetUrlCallback -> IO (FunPtr ChangeTargetUrlCallback)
{#fun awe_webview_set_callback_change_target_url { id `WebView', id `FunPtr ChangeTargetUrlCallback' } -> `()' #}

type OpenExternalLinkCallback = WebView -> AweString -> AweString -> IO()
foreign import ccall "wrapper" mkOpenExternalLinkCallback :: OpenExternalLinkCallback -> IO (FunPtr OpenExternalLinkCallback)
{#fun awe_webview_set_callback_open_external_link { id `WebView', id `FunPtr OpenExternalLinkCallback' } -> `()' #}

type RequestDownloadCallback = WebView -> AweString -> IO()
foreign import ccall "wrapper" mkRequestDownloadCallback :: RequestDownloadCallback -> IO (FunPtr RequestDownloadCallback)
{#fun awe_webview_set_callback_request_download { id `WebView', id `FunPtr RequestDownloadCallback' } -> `()' #}

type WebViewCrashedCallback = WebView -> IO()
foreign import ccall "wrapper" mkWebViewCrashedCallback :: WebViewCrashedCallback -> IO (FunPtr WebViewCrashedCallback)
{#fun awe_webview_set_callback_web_view_crashed { id `WebView', id `FunPtr WebViewCrashedCallback' } -> `()' #}

type PluginCrashedCallback = WebView -> AweString -> IO()
foreign import ccall "wrapper" mkPluginCrashedCallback :: PluginCrashedCallback -> IO (FunPtr PluginCrashedCallback)
{#fun awe_webview_set_callback_plugin_crashed { id `WebView', id `FunPtr PluginCrashedCallback' } -> `()' #}

type RequestMoveCallback = WebView -> CInt -> CInt -> IO()
foreign import ccall "wrapper" mkRequestMoveCallback :: RequestMoveCallback -> IO (FunPtr RequestMoveCallback)
{#fun awe_webview_set_callback_request_move { id `WebView', id `FunPtr RequestMoveCallback' } -> `()' #}

type GetPageContentsCallback = WebView -> AweString -> AweString -> IO()
foreign import ccall "wrapper" mkGetPageContentsCallback :: GetPageContentsCallback -> IO (FunPtr GetPageContentsCallback)
{#fun awe_webview_set_callback_get_page_contents { id `WebView', id `FunPtr GetPageContentsCallback' } -> `()' #}

type DomReadyCallback = WebView -> IO()
foreign import ccall "wrapper" mkDomReadyCallback :: DomReadyCallback -> IO (FunPtr DomReadyCallback)
{#fun awe_webview_set_callback_dom_ready { id `WebView', id `FunPtr DomReadyCallback' } -> `()' #}

type RequestFileChooserCallback = WebView -> CUInt {-Bool-} -> AweString -> AweString -> IO()
foreign import ccall "wrapper" mkRequestFileChooserCallback :: RequestFileChooserCallback -> IO (FunPtr RequestFileChooserCallback)
{#fun awe_webview_set_callback_request_file_chooser { id `WebView', id `FunPtr RequestFileChooserCallback' } -> `()' #}

type GetScrollDataCallback = WebView -> CInt -> CInt -> CInt -> CInt -> CInt -> IO()
foreign import ccall "wrapper" mkGetScrollDataCallback :: GetScrollDataCallback -> IO (FunPtr GetScrollDataCallback)
{#fun awe_webview_set_callback_get_scroll_data { id `WebView', id `FunPtr GetScrollDataCallback' } -> `()' #}

type JsConsoleMessageCallback = WebView -> AweString -> CInt -> AweString -> IO()
foreign import ccall "wrapper" mkJsConsoleMessageCallback :: JsConsoleMessageCallback -> IO (FunPtr JsConsoleMessageCallback)
{#fun awe_webview_set_callback_js_console_message { id `WebView', id `FunPtr JsConsoleMessageCallback' } -> `()' #}

type GetFindResultsCallback = WebView -> CInt -> CInt -> Ptr Rect -> CInt -> CUInt {-Bool-} -> IO()
foreign import ccall "wrapper" mkGetFindResultsCallback :: GetFindResultsCallback -> IO (FunPtr GetFindResultsCallback)
foreign import ccall safe "Graphics/UI/Awesomium/Raw.chs.h awe_webview_set_callback_get_find_results"
    awe_webview_set_callback_get_find_results :: WebView -> FunPtr GetFindResultsCallback -> IO ()

type UpdateImeCallback = WebView -> CInt {-ImeState-} -> Ptr Rect -> IO()
foreign import ccall "wrapper" mkUpdateImeCallback :: UpdateImeCallback -> IO (FunPtr UpdateImeCallback)
foreign import ccall safe "Graphics/UI/Awesomium/Raw.chs.h awe_webview_set_callback_get_find_results"
    awe_webview_set_callback_update_ime :: WebView -> FunPtr UpdateImeCallback -> IO ()

type ShowContextMenuCallback = WebView -> CInt -> CInt -> CInt {-MediaType-} -> CInt -> AweString -> AweString -> AweString -> AweString -> AweString -> CUInt {-Bool-} -> CInt -> IO()
foreign import ccall "wrapper" mkShowContextMenuCallback :: ShowContextMenuCallback -> IO (FunPtr ShowContextMenuCallback)
{#fun awe_webview_set_callback_show_context_menu { id `WebView', id `FunPtr ShowContextMenuCallback' } -> `()' #}

type RequestLoginCallback = WebView -> CInt -> AweString -> CUInt {-Bool-} -> AweString -> AweString -> AweString -> IO()
foreign import ccall "wrapper" mkRequestLoginCallback :: RequestLoginCallback -> IO (FunPtr RequestLoginCallback)
{#fun awe_webview_set_callback_request_login { id `WebView', id `FunPtr RequestLoginCallback' } -> `()' #}

type ChangeHistoryCallback = WebView -> CInt -> CInt -> IO()
foreign import ccall "wrapper" mkChangeHistoryCallback :: ChangeHistoryCallback -> IO (FunPtr ChangeHistoryCallback)
{#fun awe_webview_set_callback_change_history { id `WebView', id `FunPtr ChangeHistoryCallback' } -> `()' #}

type FinishResizeCallback = WebView -> CInt -> CInt -> IO()
foreign import ccall "wrapper" mkFinishResizeCallback :: FinishResizeCallback -> IO (FunPtr FinishResizeCallback)
{#fun awe_webview_set_callback_finish_resize { id `WebView', id `FunPtr FinishResizeCallback' } -> `()' #}

type ShowJavascriptDialogCallback = WebView -> CInt -> CInt -> AweString -> AweString -> AweString -> IO()
foreign import ccall "wrapper" mkShowJavascriptDialogCallback :: ShowJavascriptDialogCallback -> IO (FunPtr ShowJavascriptDialogCallback)
{#fun awe_webview_set_callback_show_javascript_dialog { id `WebView', id `FunPtr ShowJavascriptDialogCallback' } -> `()' #}

{-----------------------
 - JS Value Functions  -
 -----------------------}

data JSValueType
   = JSValueTypeNull
   | JSValueTypeBoolean
   | JSValueTypeInteger
   | JSValueTypeDouble
   | JSValueTypeString
   | JSValueTypeObject
   | JSValueTypeArray
   deriving (Eq, Enum)

{#fun awe_jsvalue_create_null_value { } -> `JSValue' id #}
{#fun awe_jsvalue_create_bool_value { `Bool' } -> `JSValue' id #}
{#fun awe_jsvalue_create_integer_value { `Int' } -> `JSValue' id #}
{#fun awe_jsvalue_create_double_value { `Double' } -> `JSValue' id #}
{#fun awe_jsvalue_create_string_value { withAweString* `String' } -> `JSValue' id #}
{#fun awe_jsvalue_create_object_value { id `JSObject' } -> `JSValue' id #}
{#fun awe_jsvalue_create_array_value { id `JSArray' } -> `JSValue' id #}
{#fun awe_jsvalue_destroy { id `JSValue' } -> `()' #}
{#fun awe_jsvalue_get_type { id `JSValue' } -> `JSValueType' cToEnum #}
{#fun awe_jsvalue_to_string { id `JSValue' } -> `String' fromAweStringDestroy* #}
{#fun awe_jsvalue_to_integer { id `JSValue' } -> `Int' #}
{#fun awe_jsvalue_to_double { id `JSValue' } -> `Double' #}
{#fun awe_jsvalue_to_boolean { id `JSValue' } -> `Bool' #}
{#fun awe_jsvalue_get_array { id `JSValue' } -> `JSArray' id #}
{#fun awe_jsvalue_get_object { id `JSValue' } -> `JSObject' id #}
{#fun awe_jsarray_create { id `Ptr JSValue', fromIntegral `Int' } -> `JSArray' id #}
{#fun awe_jsarray_destroy { id `JSArray' } -> `()' #}
{#fun awe_jsarray_get_size { id `JSArray' } -> `Int' fromIntegral #}
{#fun awe_jsarray_get_element { id `JSArray', fromIntegral `Int' } -> `JSValue' id #}

{-----------------------------
 - JS Value Object Functions -
 -----------------------------}

{#fun awe_jsobject_create { } -> `JSObject' id #}
{#fun awe_jsobject_destroy { id `JSObject' } -> `()' #}
{#fun awe_jsobject_has_property { id `JSObject', withAweString* `String' } -> `Bool' #}
{#fun awe_jsobject_get_property { id `JSObject', withAweString* `String' } -> `JSValue' id #}
{#fun awe_jsobject_set_property { id `JSObject', withAweString* `String', id `JSValue' } -> `()' #}
{#fun awe_jsobject_get_size { id `JSObject' } -> `Int' fromIntegral #}
{#fun awe_jsobject_get_keys { id `JSObject' } -> `JSArray' id #}

{---------------------------
 - Render Buffer Functions -
 ---------------------------}

{#fun awe_renderbuffer_get_width { id `RenderBuffer' } -> `Int' #}
{#fun awe_renderbuffer_get_height { id `RenderBuffer' } -> `Int' #}
{#fun awe_renderbuffer_get_rowspan { id `RenderBuffer' } -> `Int' #}
{#fun awe_renderbuffer_get_buffer { id `RenderBuffer' } -> `Ptr CUChar' id #}
-- {#fun awe_renderbuffer_copy_to { id `RenderBuffer', unsigned char* dest_buffer, `Int', `Int', `Bool', `Bool' } -> `()' #}
-- {#fun awe_renderbuffer_copy_to_float { id `RenderBuffer', float* dest_buffer } -> `()' #}
{#fun awe_renderbuffer_save_to_png { id `RenderBuffer', withAweString* `String', `Bool' } -> `Bool' #}
{#fun awe_renderbuffer_save_to_jpeg { id `RenderBuffer', withAweString* `String', `Int' } -> `Bool' #}
{#fun awe_renderbuffer_get_alpha_at_point { id `RenderBuffer', `Int', `Int' } -> `Int' #}
{#fun awe_renderbuffer_flush_alpha { id `RenderBuffer' } -> `()' #}

{------------------------
 - Resource Interceptor -
 ------------------------}

-- {#fun awe_webview_set_callback_resource_request { id `WebView', resource_response* (*callback } -> `()' #}
-- {#fun awe_webview_set_callback_resource_response { id `WebView', void (*callback } -> `()' #}
-- {#fun awe_resource_response_create { size_t num_bytes, unsigned char* buffer, withAweString* `String' } -> `ResourceResponse' id #}
{#fun awe_resource_response_create_from_file { withAweString* `String' } -> `ResourceResponse' id #}

{------------------------
 - Resource Request     -
 ------------------------}

{#fun awe_resource_request_cancel { id `ResourceRequest' } -> `()' #}
{#fun awe_resource_request_get_url { id `ResourceRequest' } -> `String' fromAweStringDestroy* #}
{#fun awe_resource_request_get_method { id `ResourceRequest' } -> `String' fromAweStringDestroy* #}
{#fun awe_resource_request_set_method { id `ResourceRequest', withAweString* `String' } -> `()' #}
{#fun awe_resource_request_get_referrer { id `ResourceRequest' } -> `String' fromAweStringDestroy* #}
{#fun awe_resource_request_set_referrer { id `ResourceRequest', withAweString* `String' } -> `()' #}
{#fun awe_resource_request_get_extra_headers { id `ResourceRequest' } -> `String' fromAweStringDestroy* #}
{#fun awe_resource_request_set_extra_headers { id `ResourceRequest', withAweString* `String' } -> `()' #}
{#fun awe_resource_request_append_extra_header { id `ResourceRequest', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun awe_resource_request_get_num_upload_elements { id `ResourceRequest' } -> `Int' fromIntegral #}
{#fun awe_resource_request_get_upload_element { id `ResourceRequest', fromIntegral `Int' } -> `UploadElement' id #}
{#fun awe_resource_request_clear_upload_elements { id `ResourceRequest' } -> `()' #}
{#fun awe_resource_request_append_upload_file_path { id `ResourceRequest', withAweString* `String' } -> `()' #}
{#fun awe_resource_request_append_upload_bytes { id `ResourceRequest', withAweString* `String' } -> `()' #}

{------------------------
 - Upload Element       -
 ------------------------}

{#fun awe_upload_element_is_file_path { id `UploadElement' } -> `Bool' #}
{#fun awe_upload_element_is_bytes { id `UploadElement' } -> `Bool' #}
{#fun awe_upload_element_get_bytes { id `UploadElement' } -> `String' fromAweStringDestroy* #}
{#fun awe_upload_element_get_file_path { id `UploadElement' } -> `String' fromAweStringDestroy* #}

{------------------------
 - History Query Result -
 ------------------------}

{#fun awe_history_query_result_destroy { id `HistoryQueryResult' } -> `()' #}
{#fun awe_history_query_result_get_size { id `HistoryQueryResult' } -> `Int' fromIntegral #}
{#fun awe_history_query_result_get_entry_at_index { id `HistoryQueryResult', fromIntegral `Int' } -> `HistoryEntry' id #}

{------------------------
 - History Entry        -
 ------------------------}

{#fun awe_history_entry_destroy { id `HistoryEntry' } -> `()' #}
{#fun awe_history_entry_get_url { id `HistoryEntry' } -> `String' fromAweStringDestroy* #}
{#fun awe_history_entry_get_title { id `HistoryEntry' } -> `String' fromAweStringDestroy* #}
{#fun awe_history_entry_get_visit_time { id `HistoryEntry' } -> `Double' #}
{#fun awe_history_entry_get_visit_count { id `HistoryEntry' } -> `Int' #}

