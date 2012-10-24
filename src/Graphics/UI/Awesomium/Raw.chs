{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.UI.Awesomium.Raw where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
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

{#enum loglevel as LogLevel {underscoreToCase} deriving (Show, Read, Eq)#}
{#enum mousebutton as MouseButton {underscoreToCase}#}
{#enum url_filtering_mode as UrlFilteringMode {underscoreToCase}#}
{#enum webkey_type as WebkeyType {underscoreToCase}#}
{#enum webkey_modifiers as WebkeyModifiers {underscoreToCase}#}
{#enum cursor_type as CursorType {underscoreToCase}#}
{#enum ime_state as ImeState {underscoreToCase}#}
{#enum media_type as MediaType {underscoreToCase}#}

{- TODO
typedef enum _awe_media_state
{
    AWE_MEDIA_STATE_NONE = 0x0,
    AWE_MEDIA_STATE_ERROR = 0x1,
    AWE_MEDIA_STATE_PAUSED = 0x2,
    AWE_MEDIA_STATE_MUTED = 0x4,
    AWE_MEDIA_STATE_LOOP = 0x8,
    AWE_MEDIA_STATE_CAN_SAVE = 0x10,
    AWE_MEDIA_STATE_HAS_AUDIO = 0x20
};

enum _awe_can_edit_flags
{
    AWE_CAN_EDIT_NOTHING = 0x0,
    AWE_CAN_UNDO = 0x1,
    AWE_CAN_REDO = 0x2,
    AWE_CAN_CUT = 0x4,
    AWE_CAN_COPY = 0x8,
    AWE_CAN_PASTE = 0x10,
    AWE_CAN_DELETE = 0x20,
    AWE_CAN_SELECT_ALL = 0x40
};

enum _awe_dialog_flags
{
    AWE_DIALOG_HAS_OK_BUTTON = 0x1,
    AWE_DIALOG_HAS_CANCEL_BUTTON = 0x2,
    AWE_DIALOG_HAS_PROMPT_FIELD = 0x4,
    AWE_DIALOG_HAS_MESSAGE = 0x8
};

typedef struct _awe_webkeyboardevent
{
    awe_webkey_type type;
    int modifiers;
    int virtual_key_code;
    int native_key_code;
    wchar16 text[4];
    wchar16 unmodified_text[4];
    `Bool';
} awe_webkeyboardevent;

typedef struct _awe_rect
{
    int x, y, width, height;
} awe_rect;

#ifdef _WIN32
{#fun is_child_process { HINSTANCE hInstance } -> `Bool' #}
{#fun child_process_main { HINSTANCE hInstance } -> `Int' #}
#else
{#fun is_child_process { `Int', char** argv } -> `Bool' #}
{#fun child_process_main { `Int', char** argv } -> `Int' #}
#endif
-}

{-----------------------
 - AweString Functions -
 -----------------------}

{#fun awe_string_empty { } -> `AweString' id #}
-- {#fun awe_string_create_from_ascii { `String'& } -> `AweString' id #}
-- {#fun awe_string_create_from_wide { `String'& } -> `AweString' id #}
{#fun awe_string_create_from_utf8 { `String'& } -> `AweString' id #}
-- {#fun awe_string_create_from_utf16 { `String'& } -> `AweString' id #}
{#fun awe_string_destroy { id `AweString' } -> `()' #}
{#fun awe_string_get_length { id `AweString' } -> `Int' fromIntegral #}
-- {#fun awe_string_get_utf16 { id `AweString' } -> `String' #}
-- {#fun awe_string_to_wide { id `AweString' , `String'& } -> `Int' #}
-- {#fun awe_string_to_utf8 { id `AweString' , id- `String' peekCString*, id `()' } -> `Int' #}

foreign import ccall "Graphics/UI/Awesomium/Raw.chs.h awe_string_to_utf8"
    awe_string_to_utf8'_ :: AweString -> Ptr CChar -> CULong -> IO CInt

awe_string_to_utf8 :: AweString -> IO (String)
awe_string_to_utf8 a1 = do
    len <- awe_string_get_length a1
    allocaBytes len $ \buf -> do
        awe_string_to_utf8'_ a1 buf (cIntConv len)
        peekCStringLen (buf, (cIntConv len))

fromAweString :: AweString -> IO (String)
fromAweString = awe_string_to_utf8

fromAweStringDestroy :: AweString -> IO (String)
fromAweStringDestroy as = do
    res <- awe_string_to_utf8 as
    awe_string_destroy as
    return res

withAweString :: String -> (AweString -> IO b) -> IO b
withAweString str f = do
    -- bracket (awe_string_create_from_utf8 str) awe_string_destroy f
    as <- awe_string_create_from_utf8 str
    res <- f as
    awe_string_destroy as
    return res

{-----------------------
 - Web Core Functions  -
 -----------------------}

{#fun webcore_initialize as ^ { `Bool', `Bool', `Bool', withAweString* `String', withAweString* `String', withAweString* `String', withAweString* `String', withAweString* `String', cFromEnum `LogLevel', `Bool', withAweString* `String', `Bool', withAweString* `String', withAweString* `String', withAweString* `String', withAweString* `String', withAweString* `String', withAweString* `String', `Bool', `Int', `Bool', `Bool', withAweString* `String' } -> `()' #}
{#fun webcore_initialize_default as ^ { } -> `()' #}
{#fun webcore_shutdown as ^ { } -> `()' #}
{#fun webcore_set_base_directory as ^ { withAweString* `String' } -> `()' #}
{#fun webcore_create_webview as ^ { `Int', `Int', `Bool' } -> `WebView' id #}
{#fun webcore_set_custom_response_page as ^ { `Int', withAweString* `String' } -> `()' #}
{#fun webcore_update as ^ { } -> `()' #}
{#fun webcore_get_base_directory as ^ { } -> `AweString' id #}
{#fun webcore_are_plugins_enabled as ^ { } -> `Bool' #}
{#fun webcore_clear_cache as ^ { } -> `()' #}
{#fun webcore_clear_cookies as ^ { } -> `()' #}
{#fun webcore_set_cookie as ^ { withAweString* `String', withAweString* `String', `Bool', `Bool' } -> `()' #}
{#fun webcore_get_cookies as ^ { withAweString* `String', `Bool' } -> `AweString' id #}
{#fun webcore_delete_cookie as ^ { withAweString* `String', withAweString* `String' } -> `()' #}
{#fun webcore_set_suppress_printer_dialog as ^ { `Bool' } -> `()' #}
{#fun webcore_query_history as ^ { withAweString* `String', `Int', `Int' } -> `HistoryQueryResult' id #}

{-----------------------
 - Web View Functions  -
 -----------------------}

{#fun webview_destroy as ^ { id `WebView' } -> `()' #}
{#fun webview_load_url as ^ { id `WebView', withAweString* `String', withAweString* `String', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun webview_load_html as ^ { id `WebView', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun webview_load_file as ^ { id `WebView', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun webview_get_url as ^ { id `WebView' } -> `String' fromAweStringDestroy* #}
{#fun webview_go_to_history_offset as ^ { id `WebView', `Int' } -> `()' #}
{#fun webview_get_history_back_count as ^ { id `WebView' } -> `Int' #}
{#fun webview_get_history_forward_count as ^ { id `WebView' } -> `Int' #}
{#fun webview_stop as ^ { id `WebView' } -> `()' #}
{#fun webview_reload as ^ { id `WebView' } -> `()' #}
{#fun webview_execute_javascript as ^ { id `WebView', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun webview_execute_javascript_with_result as ^ { id `WebView', withAweString* `String', withAweString* `String', `Int' } -> `JSValue' id #}
{#fun webview_call_javascript_function as ^ { id `WebView', withAweString* `String', withAweString* `String', id `JSArray', withAweString* `String' } -> `()' #}
{#fun webview_create_object as ^ { id `WebView', withAweString* `String' } -> `()' #}
{#fun webview_destroy_object as ^ { id `WebView', withAweString* `String' } -> `()' #}
{#fun webview_set_object_property as ^ { id `WebView', withAweString* `String', withAweString* `String', id `JSValue' } -> `()' #}
{#fun webview_set_object_callback as ^ { id `WebView', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun webview_is_loading_page as ^ { id `WebView' } -> `Bool' #}
{#fun webview_is_dirty as ^ { id `WebView' } -> `Bool' #}
-- {#fun webview_get_dirty_bounds as ^ { id `WebView' } -> rect #}
{#fun webview_render as ^ { id `WebView' } -> `RenderBuffer' id #}
{#fun webview_pause_rendering as ^ { id `WebView' } -> `()' #}
{#fun webview_resume_rendering as ^ { id `WebView' } -> `()' #}
{#fun webview_inject_mouse_move as ^ { id `WebView', `Int', `Int' } -> `()' #}
{#fun webview_inject_mouse_down as ^ { id `WebView', cFromEnum `MouseButton' } -> `()' #}
{#fun webview_inject_mouse_up as ^ { id `WebView', cFromEnum `MouseButton' } -> `()' #}
{#fun webview_inject_mouse_wheel as ^ { id `WebView', `Int', `Int' } -> `()' #}
-- {#fun webview_inject_keyboard_event as ^ { id `WebView', webkeyboardevent key_event } -> `()' #}

#ifdef _WIN32
-- {#fun webview_inject_keyboard_event_win as ^ { id `WebView', UINT msg, WPARAM wparam, LPARAM lparam } -> `()' #}
#endif

{#fun webview_cut as ^ { id `WebView' } -> `()' #}
{#fun webview_copy as ^ { id `WebView' } -> `()' #}
{#fun webview_paste as ^ { id `WebView' } -> `()' #}
{#fun webview_select_all as ^ { id `WebView' } -> `()' #}
{#fun webview_copy_image_at as ^ { id `WebView', `Int', `Int' } -> `()' #}
{#fun webview_set_zoom as ^ { id `WebView', `Int' } -> `()' #}
{#fun webview_reset_zoom as ^ { id `WebView' } -> `()' #}
{#fun webview_get_zoom as ^ { id `WebView' } -> `Int' #}
{#fun webview_get_zoom_for_host as ^ { id `WebView', withAweString* `String' } -> `Int' #}
{#fun webview_resize as ^ { id `WebView', `Int', `Int', `Bool', `Int' } -> `Bool' #}
{#fun webview_is_resizing as ^ { id `WebView' } -> `Bool' #}
{#fun webview_unfocus as ^ { id `WebView' } -> `()' #}
{#fun webview_focus as ^ { id `WebView' } -> `()' #}
{#fun webview_set_transparent as ^ { id `WebView', `Bool' } -> `()' #}
{#fun webview_is_transparent as ^ { id `WebView' } -> `Bool' #}
{#fun webview_set_url_filtering_mode as ^ { id `WebView', cFromEnum `UrlFilteringMode' } -> `()' #}
{#fun webview_add_url_filter as ^ { id `WebView', withAweString* `String' } -> `()' #}
{#fun webview_clear_all_url_filters as ^ { id `WebView' } -> `()' #}
-- TODO: webview_set_header_definition :: WebView -> String -> [(String, String)] -> IO ()
-- {#fun webview_set_header_definition as ^ { id `WebView', withAweString* `String', fromIntegral `Int' , id `Ptr AweString', id `Ptr AweString' } -> `()' #}
{#fun webview_add_header_rewrite_rule as ^ { id `WebView', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun webview_remove_header_rewrite_rule as ^ { id `WebView', withAweString* `String' } -> `()' #}
{#fun webview_remove_header_rewrite_rules_by_definition_name as ^ { id `WebView', withAweString* `String' } -> `()' #}
{#fun webview_choose_file as ^ { id `WebView', withAweString* `String' } -> `()' #}
{#fun webview_print as ^ { id `WebView' } -> `()' #}
{#fun webview_request_scroll_data as ^ { id `WebView', withAweString* `String' } -> `()' #}
{#fun webview_find as ^ { id `WebView', `Int', withAweString* `String', `Bool', `Bool', `Bool' } -> `()' #}
{#fun webview_stop_find as ^ { id `WebView', `Bool' } -> `()' #}
{#fun webview_translate_page as ^ { id `WebView', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun webview_activate_ime as ^ { id `WebView', `Bool' } -> `()' #}
{#fun webview_set_ime_composition as ^ { id `WebView', withAweString* `String', `Int', `Int', `Int' } -> `()' #}
{#fun webview_confirm_ime_composition as ^ { id `WebView', withAweString* `String' } -> `()' #}
{#fun webview_cancel_ime_composition as ^ { id `WebView' } -> `()' #}
{#fun webview_login as ^ { id `WebView', `Int', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun webview_cancel_login as ^ { id `WebView', `Int' } -> `()' #}
{#fun webview_close_javascript_dialog as ^ { id `WebView', `Int', `Bool', withAweString* `String' } -> `()' #}
{-
{#fun webview_set_callback_begin_navigation as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_begin_loading as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_finish_loading as ^ { id `WebView', void (*callback } -> `()' #}
-}
type JSCallback = WebView -> AweString -> AweString -> JSArray -> IO()
foreign import ccall "wrapper"
    mkCallback :: JSCallback -> IO (FunPtr JSCallback)
{#fun webview_set_callback_js_callback as ^ { id `WebView', id `FunPtr JSCallback' } -> `()' #}
{-
{#fun webview_set_callback_receive_title as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_change_tooltip as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_change_cursor as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_change_keyboard_focus as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_change_target_url as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_open_external_link as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_request_download as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_web_view_crashed as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_plugin_crashed as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_request_move as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_get_page_contents as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_dom_ready as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_request_file_chooser as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_get_scroll_data as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_js_console_message as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_get_find_results as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_update_ime as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_show_context_menu as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_request_login as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_change_history as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_finish_resize as ^ { id `WebView', void (*callback } -> `()' #}
{#fun webview_set_callback_show_javascript_dialog as ^ { id `WebView', void (*callback } -> `()' #}
-}

{-----------------------
 - JS Value Functions  -
 -----------------------}

{#enum jsvalue_type as JSValueType {underscoreToCase}#}

{#fun jsvalue_create_null_value as ^ { } -> `JSValue' id #}
{#fun jsvalue_create_bool_value as ^ { `Bool' } -> `JSValue' id #}
{#fun jsvalue_create_integer_value as ^ { `Int' } -> `JSValue' id #}
{#fun jsvalue_create_double_value as ^ { `Double' } -> `JSValue' id #}
{#fun jsvalue_create_string_value as ^ { withAweString* `String' } -> `JSValue' id #}
{#fun jsvalue_create_object_value as ^ { id `JSObject' } -> `JSValue' id #}
{#fun jsvalue_create_array_value as ^ { id `JSArray' } -> `JSValue' id #}
{#fun jsvalue_destroy as ^ { id `JSValue' } -> `()' #}
{#fun jsvalue_get_type as ^ { id `JSValue' } -> `JSValueType' cToEnum #}
{#fun jsvalue_to_string as ^ { id `JSValue' } -> `String' fromAweStringDestroy* #}
{#fun jsvalue_to_integer as ^ { id `JSValue' } -> `Int' #}
{#fun jsvalue_to_double as ^ { id `JSValue' } -> `Double' #}
{#fun jsvalue_to_boolean as ^ { id `JSValue' } -> `Bool' #}
{#fun jsvalue_get_array as ^ { id `JSValue' } -> `JSArray' id #}
{#fun jsvalue_get_object as ^ { id `JSValue' } -> `JSObject' id #}
{#fun jsarray_create as ^ { id `Ptr JSValue', fromIntegral `Int' } -> `JSArray' id #}
{#fun jsarray_destroy as ^ { id `JSArray' } -> `()' #}
{#fun jsarray_get_size as ^ { id `JSArray' } -> `Int' fromIntegral #}
{#fun jsarray_get_element as ^ { id `JSArray', fromIntegral `Int' } -> `JSValue' id #}

{-----------------------------
 - JS Value Object Functions -
 -----------------------------}

{#fun jsobject_create as ^ { } -> `JSObject' id #}
{#fun jsobject_destroy as ^ { id `JSObject' } -> `()' #}
{#fun jsobject_has_property as ^ { id `JSObject', withAweString* `String' } -> `Bool' #}
{#fun jsobject_get_property as ^ { id `JSObject', withAweString* `String' } -> `JSValue' id #}
{#fun jsobject_set_property as ^ { id `JSObject', withAweString* `String', id `JSValue' } -> `()' #}
{#fun jsobject_get_size as ^ { id `JSObject' } -> `Int' fromIntegral #}
{#fun jsobject_get_keys as ^ { id `JSObject' } -> `JSArray' id #}

{---------------------------
 - Render Buffer Functions -
 ---------------------------}

{#fun renderbuffer_get_width as ^ { id `RenderBuffer' } -> `Int' #}
{#fun renderbuffer_get_height as ^ { id `RenderBuffer' } -> `Int' #}
{#fun renderbuffer_get_rowspan as ^ { id `RenderBuffer' } -> `Int' #}
{#fun renderbuffer_get_buffer as ^ { id `RenderBuffer' } -> `Ptr CUChar' id #}
-- {#fun renderbuffer_copy_to as ^ { id `RenderBuffer', unsigned char* dest_buffer, `Int', `Int', `Bool', `Bool' } -> `()' #}
-- {#fun renderbuffer_copy_to_float as ^ { id `RenderBuffer', float* dest_buffer } -> `()' #}
{#fun renderbuffer_save_to_png as ^ { id `RenderBuffer', withAweString* `String', `Bool' } -> `Bool' #}
{#fun renderbuffer_save_to_jpeg as ^ { id `RenderBuffer', withAweString* `String', `Int' } -> `Bool' #}
-- {#fun renderbuffer_get_alpha_at_point as ^ { id `RenderBuffer', `Int', `Int' } -> unsigned char #}
{#fun renderbuffer_flush_alpha as ^ { id `RenderBuffer' } -> `()' #}

{------------------------
 - Resource Interceptor -
 ------------------------}

-- {#fun webview_set_callback_resource_request as ^ { id `WebView', resource_response* (*callback } -> `()' #}
-- {#fun webview_set_callback_resource_response as ^ { id `WebView', void (*callback } -> `()' #}
-- {#fun resource_response_create as ^ { size_t num_bytes, unsigned char* buffer, withAweString* `String' } -> `ResourceResponse' id #}
{#fun resource_response_create_from_file as ^ { withAweString* `String' } -> `ResourceResponse' id #}

{------------------------
 - Resource Request     -
 ------------------------}

{#fun resource_request_cancel as ^ { id `ResourceRequest' } -> `()' #}
{#fun resource_request_get_url as ^ { id `ResourceRequest' } -> `String' fromAweStringDestroy* #}
{#fun resource_request_get_method as ^ { id `ResourceRequest' } -> `String' fromAweStringDestroy* #}
{#fun resource_request_set_method as ^ { id `ResourceRequest', withAweString* `String' } -> `()' #}
{#fun resource_request_get_referrer as ^ { id `ResourceRequest' } -> `String' fromAweStringDestroy* #}
{#fun resource_request_set_referrer as ^ { id `ResourceRequest', withAweString* `String' } -> `()' #}
{#fun resource_request_get_extra_headers as ^ { id `ResourceRequest' } -> `String' fromAweStringDestroy* #}
{#fun resource_request_set_extra_headers as ^ { id `ResourceRequest', withAweString* `String' } -> `()' #}
{#fun resource_request_append_extra_header as ^ { id `ResourceRequest', withAweString* `String', withAweString* `String' } -> `()' #}
{#fun resource_request_get_num_upload_elements as ^ { id `ResourceRequest' } -> `Int' fromIntegral #}
{#fun resource_request_get_upload_element as ^ { id `ResourceRequest', fromIntegral `Int' } -> `UploadElement' id #}
{#fun resource_request_clear_upload_elements as ^ { id `ResourceRequest' } -> `()' #}
{#fun resource_request_append_upload_file_path as ^ { id `ResourceRequest', withAweString* `String' } -> `()' #}
{#fun resource_request_append_upload_bytes as ^ { id `ResourceRequest', withAweString* `String' } -> `()' #}

{------------------------
 - Upload Element       -
 ------------------------}

{#fun upload_element_is_file_path as ^ { id `UploadElement' } -> `Bool' #}
{#fun upload_element_is_bytes as ^ { id `UploadElement' } -> `Bool' #}
{#fun upload_element_get_bytes as ^ { id `UploadElement' } -> `String' fromAweStringDestroy* #}
{#fun upload_element_get_file_path as ^ { id `UploadElement' } -> `String' fromAweStringDestroy* #}

{------------------------
 - History Query Result -
 ------------------------}

{#fun history_query_result_destroy as ^ { id `HistoryQueryResult' } -> `()' #}
{#fun history_query_result_get_size as ^ { id `HistoryQueryResult' } -> `Int' fromIntegral #}
{#fun history_query_result_get_entry_at_index as ^ { id `HistoryQueryResult', fromIntegral `Int' } -> `HistoryEntry' id #}

{------------------------
 - History Entry        -
 ------------------------}

{#fun history_entry_destroy as ^ { id `HistoryEntry' } -> `()' #}
{#fun history_entry_get_url as ^ { id `HistoryEntry' } -> `String' fromAweStringDestroy* #}
{#fun history_entry_get_title as ^ { id `HistoryEntry' } -> `String' fromAweStringDestroy* #}
{#fun history_entry_get_visit_time as ^ { id `HistoryEntry' } -> `Double' #}
{#fun history_entry_get_visit_count as ^ { id `HistoryEntry' } -> `Int' #}

