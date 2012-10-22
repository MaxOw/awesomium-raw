{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.UI.Awesomium.Raw where

import Foreign.C.Types
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
data JSArray 
{#pointer *jsarray as JSArray -> DJSArray #}
-- | JSObject instance
data JSObject 
{#pointer *jsobject as JSObject -> DJSObject #}
-- | RenderBuffer instance, owned by the WebView
data RenderBuffer 
{#pointer *renderbuffer as RenderBuffer -> DRenderBuffer #}
-- | HeaderDefinition instance
data HeaderDefinition 
{#pointer *header_definition as HeaderDefinition -> DHeaderDefinition #}
-- | ResourceResponse instance
data ResourceResponse 
{#pointer *resource_response as ResourceResponse -> DResourceResponse #}
-- | ResourceRequest instance
data ResourceRequest 
{#pointer *resource_request as ResourceRequest -> DResourceRequest #}
-- | UploadElement instance
data UploadElement 
{#pointer *upload_element as UploadElement -> DUploadElement #}
-- | String instance
data DAweString
{#pointer *awe_string as AweString -> DAweString #}
-- | HistoryQueryResult instance
data HistoryQueryResult 
{#pointer *history_query_result as HistoryQueryResult -> DHistoryQueryResult #}
-- | HistoryEntry instance
data HistoryEntry 
{#pointer *history_entry as HistoryEntry -> DHistoryEntry #}

{#enum loglevel as LogLevel {underscoreToCase}#}
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
{#fun unsafe awe_is_child_process { HINSTANCE hInstance } -> `Bool' #}
{#fun unsafe awe_child_process_main { HINSTANCE hInstance } -> `Int' #}
#else
{#fun unsafe awe_is_child_process { `Int', char** argv } -> `Bool' #}
{#fun unsafe awe_child_process_main { `Int', char** argv } -> `Int' #}
#endif
-}

{#fun unsafe awe_string_empty { } -> `AweString' id #}
{#fun unsafe awe_string_create_from_ascii { `String'& } -> `AweString' id #}
-- {#fun unsafe awe_string_create_from_wide { `String'& } -> `AweString' id #}
{#fun unsafe awe_string_create_from_utf8 { `String'& } -> `AweString' id #}
-- {#fun unsafe awe_string_create_from_utf16 { `String'& } -> `AweString' id #}
{#fun unsafe awe_string_destroy { id `AweString' } -> `()' #}
{#fun unsafe awe_string_get_length { id `AweString' } -> `Int' fromIntegral #}
-- {#fun unsafe awe_string_get_utf16 { id `AweString' } -> `String' #}
-- {#fun unsafe awe_string_to_wide { id `AweString' , `String'& } -> `Int' #}
-- {#fun unsafe awe_string_to_utf8 { id `AweString' , alloca- `String' peekCStr* } -> `Int' #}

{-----------------------
 - Web Core Functions  -
 -----------------------}

{#fun unsafe awe_webcore_initialize { `Bool', `Bool', `Bool', id `AweString', id `AweString', id `AweString', id `AweString', id `AweString', id `LogLevel', `Bool', id `AweString', `Bool', id `AweString', id `AweString', id `AweString', id `AweString', id `AweString', id `AweString', `Bool', `Int', `Bool', `Bool', id `AweString' } -> `()' #}
{#fun unsafe awe_webcore_initialize_default { } -> `()' #}
{#fun unsafe awe_webcore_shutdown { } -> `()' #}
{#fun unsafe awe_webcore_set_base_directory { id `AweString' } -> `()' #}
{#fun unsafe awe_webcore_create_webview { `Int', `Int', `Bool' } -> `WebView' id #}
{#fun unsafe awe_webcore_set_custom_response_page { `Int', id `AweString' } -> `()' #}
{#fun unsafe awe_webcore_update { } -> `()' #}
{#fun unsafe awe_webcore_get_base_directory { } -> `AweString' id #}
{#fun unsafe awe_webcore_are_plugins_enabled { } -> `Bool' #}
{#fun unsafe awe_webcore_clear_cache { } -> `()' #}
{#fun unsafe awe_webcore_clear_cookies { } -> `()' #}
{#fun unsafe awe_webcore_set_cookie { id `AweString', id `AweString', `Bool', `Bool' } -> `()' #}
{#fun unsafe awe_webcore_get_cookies { id `AweString', `Bool' } -> `AweString' id #}
{#fun unsafe awe_webcore_delete_cookie { id `AweString', id `AweString' } -> `()' #}
{#fun unsafe awe_webcore_set_suppress_printer_dialog { `Bool' } -> `()' #}
{#fun unsafe awe_webcore_query_history { id `AweString', `Int', `Int' } -> `HistoryQueryResult' id #}


{-----------------------
 - Web View Functions  -
 -----------------------}

{#fun unsafe awe_webview_destroy { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_load_url { id `WebView', id `AweString', id `AweString', id `AweString', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_load_html { id `WebView', id `AweString', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_load_file { id `WebView', id `AweString', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_get_url { id `WebView' } -> `AweString' id #}
{#fun unsafe awe_webview_go_to_history_offset { id `WebView', `Int' } -> `()' #}
{#fun unsafe awe_webview_get_history_back_count { id `WebView' } -> `Int' #}
{#fun unsafe awe_webview_get_history_forward_count { id `WebView' } -> `Int' #}
{#fun unsafe awe_webview_stop { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_reload { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_execute_javascript { id `WebView', id `AweString', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_execute_javascript_with_result { id `WebView', id `AweString', id `AweString', `Int' } -> `JSValue' id #}
{#fun unsafe awe_webview_call_javascript_function { id `WebView', id `AweString', id `AweString', id `JSArray', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_create_object { id `WebView', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_destroy_object { id `WebView', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_set_object_property { id `WebView', id `AweString', id `AweString', id `JSValue' } -> `()' #}
{#fun unsafe awe_webview_set_object_callback { id `WebView', id `AweString', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_is_loading_page { id `WebView' } -> `Bool' #}
{#fun unsafe awe_webview_is_dirty { id `WebView' } -> `Bool' #}
-- {#fun unsafe awe_webview_get_dirty_bounds { id `WebView' } -> awe_rect #}
{#fun unsafe awe_webview_render { id `WebView' } -> `RenderBuffer' id #}
{#fun unsafe awe_webview_pause_rendering { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_resume_rendering { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_inject_mouse_move { id `WebView', `Int', `Int' } -> `()' #}
{#fun unsafe awe_webview_inject_mouse_down { id `WebView', id `MouseButton' } -> `()' #}
{#fun unsafe awe_webview_inject_mouse_up { id `WebView', id `MouseButton' } -> `()' #}
{#fun unsafe awe_webview_inject_mouse_wheel { id `WebView', `Int', `Int' } -> `()' #}
-- {#fun unsafe awe_webview_inject_keyboard_event { id `WebView', awe_webkeyboardevent key_event } -> `()' #}

#ifdef _WIN32
-- {#fun unsafe awe_webview_inject_keyboard_event_win { id `WebView', UINT msg, WPARAM wparam, LPARAM lparam } -> `()' #}
#endif

{#fun unsafe awe_webview_cut { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_copy { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_paste { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_select_all { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_copy_image_at { id `WebView', `Int', `Int' } -> `()' #}
{#fun unsafe awe_webview_set_zoom { id `WebView', `Int' } -> `()' #}
{#fun unsafe awe_webview_reset_zoom { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_get_zoom { id `WebView' } -> `Int' #}
{#fun unsafe awe_webview_get_zoom_for_host { id `WebView', id `AweString' } -> `Int' #}
{#fun unsafe awe_webview_resize { id `WebView', `Int', `Int', `Bool', `Int' } -> `Bool' #}
{#fun unsafe awe_webview_is_resizing { id `WebView' } -> `Bool' #}
{#fun unsafe awe_webview_unfocus { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_focus { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_set_transparent { id `WebView', `Bool' } -> `()' #}
{#fun unsafe awe_webview_is_transparent { id `WebView' } -> `Bool' #}
{#fun unsafe awe_webview_set_url_filtering_mode { id `WebView', fromEnum `UrlFilteringMode' } -> `()' #}
{#fun unsafe awe_webview_add_url_filter { id `WebView', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_clear_all_url_filters { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_set_header_definition { id `WebView', id `AweString', fromIntegral `Int' , id `Ptr AweString', id `Ptr AweString' } -> `()' #}
{#fun unsafe awe_webview_add_header_rewrite_rule { id `WebView', id `AweString', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_remove_header_rewrite_rule { id `WebView', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_remove_header_rewrite_rules_by_definition_name { id `WebView', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_choose_file { id `WebView', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_print { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_request_scroll_data { id `WebView', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_find { id `WebView', `Int', id `AweString', `Bool', `Bool', `Bool' } -> `()' #}
{#fun unsafe awe_webview_stop_find { id `WebView', `Bool' } -> `()' #}
{#fun unsafe awe_webview_translate_page { id `WebView', id `AweString', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_activate_ime { id `WebView', `Bool' } -> `()' #}
{#fun unsafe awe_webview_set_ime_composition { id `WebView', id `AweString', `Int', `Int', `Int' } -> `()' #}
{#fun unsafe awe_webview_confirm_ime_composition { id `WebView', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_cancel_ime_composition { id `WebView' } -> `()' #}
{#fun unsafe awe_webview_login { id `WebView', `Int', id `AweString', id `AweString' } -> `()' #}
{#fun unsafe awe_webview_cancel_login { id `WebView', `Int' } -> `()' #}
{#fun unsafe awe_webview_close_javascript_dialog { id `WebView', `Int', `Bool', id `AweString' } -> `()' #}
{-
{#fun unsafe awe_webview_set_callback_begin_navigation { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_begin_loading { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_finish_loading { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_js_callback { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_receive_title { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_change_tooltip { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_change_cursor { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_change_keyboard_focus { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_change_target_url { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_open_external_link { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_request_download { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_web_view_crashed { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_plugin_crashed { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_request_move { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_get_page_contents { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_dom_ready { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_request_file_chooser { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_get_scroll_data { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_js_console_message { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_get_find_results { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_update_ime { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_show_context_menu { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_request_login { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_change_history { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_finish_resize { id `WebView', void (*callback } -> `()' #}
{#fun unsafe awe_webview_set_callback_show_javascript_dialog { id `WebView', void (*callback } -> `()' #}
-}

{-----------------------
 - JS Value Functions  -
 -----------------------}

{#enum jsvalue_type as JSValueType {underscoreToCase}#}

{#fun unsafe awe_jsvalue_create_null_value { } -> `JSValue' id #}
{#fun unsafe awe_jsvalue_create_bool_value { `Bool' } -> `JSValue' id #}
{#fun unsafe awe_jsvalue_create_integer_value { `Int' } -> `JSValue' id #}
{#fun unsafe awe_jsvalue_create_double_value { `Double' } -> `JSValue' id #}
{#fun unsafe awe_jsvalue_create_string_value { id `AweString' } -> `JSValue' id #}
{#fun unsafe awe_jsvalue_create_object_value { id `JSObject' } -> `JSValue' id #}
{#fun unsafe awe_jsvalue_create_array_value { id `JSArray' } -> `JSValue' id #}
{#fun unsafe awe_jsvalue_destroy { id `JSValue' } -> `()' #}
{#fun unsafe awe_jsvalue_get_type { id `JSValue' } -> `JSValueType' id #}
{#fun unsafe awe_jsvalue_to_string { id `JSValue' } -> `AweString' id #}
{#fun unsafe awe_jsvalue_to_integer { id `JSValue' } -> `Int' #}
{#fun unsafe awe_jsvalue_to_double { id `JSValue' } -> `Double' #}
{#fun unsafe awe_jsvalue_to_boolean { id `JSValue' } -> `Bool' #}
{#fun unsafe awe_jsvalue_get_array { id `JSValue' } -> `JSArray' id #}
{#fun unsafe awe_jsvalue_get_object { id `JSValue' } -> `JSObject' id #}
{#fun unsafe awe_jsarray_create { id `Ptr JSValue', fromIntegral `Int' } -> `JSArray' id #}
{#fun unsafe awe_jsarray_destroy { id `JSArray' } -> `()' #}
{#fun unsafe awe_jsarray_get_size { id `JSArray' } -> `Int' fromIntegral #}
{#fun unsafe awe_jsarray_get_element { id `JSArray', fromIntegral `Int' } -> `JSValue' id #}

{-----------------------------
 - JS Value Object Functions -
 -----------------------------}

{#fun unsafe awe_jsobject_create { } -> `JSObject' id #}
{#fun unsafe awe_jsobject_destroy { id `JSObject' } -> `()' #}
{#fun unsafe awe_jsobject_has_property { id `JSObject', id `AweString' } -> `Bool' #}
{#fun unsafe awe_jsobject_get_property { id `JSObject', id `AweString' } -> `JSValue' id #}
{#fun unsafe awe_jsobject_set_property { id `JSObject', id `AweString', id `JSValue' } -> `()' #}
{#fun unsafe awe_jsobject_get_size { id `JSObject' } -> `Int' fromIntegral #}
{#fun unsafe awe_jsobject_get_keys { id `JSObject' } -> `JSArray' id #}

{---------------------------
 - Render Buffer Functions -
 ---------------------------}

{#fun unsafe awe_renderbuffer_get_width { id `RenderBuffer' } -> `Int' #}
{#fun unsafe awe_renderbuffer_get_height { id `RenderBuffer' } -> `Int' #}
{#fun unsafe awe_renderbuffer_get_rowspan { id `RenderBuffer' } -> `Int' #}
{#fun unsafe awe_renderbuffer_get_buffer { id `RenderBuffer' } -> `Ptr CChar' id #}
-- {#fun unsafe awe_renderbuffer_copy_to { id `RenderBuffer', unsigned char* dest_buffer, `Int', `Int', `Bool', `Bool' } -> `()' #}
-- {#fun unsafe awe_renderbuffer_copy_to_float { id `RenderBuffer', float* dest_buffer } -> `()' #}
{#fun unsafe awe_renderbuffer_save_to_png { id `RenderBuffer', id `AweString', `Bool' } -> `Bool' #}
{#fun unsafe awe_renderbuffer_save_to_jpeg { id `RenderBuffer', id `AweString', `Int' } -> `Bool' #}
-- {#fun unsafe awe_renderbuffer_get_alpha_at_point { id `RenderBuffer', `Int', `Int' } -> unsigned char #}
{#fun unsafe awe_renderbuffer_flush_alpha { id `RenderBuffer' } -> `()' #}

{------------------------
 - Resource Interceptor -
 ------------------------}

-- {#fun unsafe awe_webview_set_callback_resource_request { id `WebView', awe_resource_response* (*callback } -> `()' #}
-- {#fun unsafe awe_webview_set_callback_resource_response { id `WebView', void (*callback } -> `()' #}
-- {#fun unsafe awe_resource_response_create { size_t num_bytes, unsigned char* buffer, id `AweString' } -> `ResourceResponse' id #}
{#fun unsafe awe_resource_response_create_from_file { id `AweString' } -> `ResourceResponse' id #}

{------------------------
 - Resource Request     -
 ------------------------}

{#fun unsafe awe_resource_request_cancel { id `ResourceRequest' } -> `()' #}
{#fun unsafe awe_resource_request_get_url { id `ResourceRequest' } -> `AweString' id #}
{#fun unsafe awe_resource_request_get_method { id `ResourceRequest' } -> `AweString' id #}
{#fun unsafe awe_resource_request_set_method { id `ResourceRequest', id `AweString' } -> `()' #}
{#fun unsafe awe_resource_request_get_referrer { id `ResourceRequest' } -> `AweString' id #}
{#fun unsafe awe_resource_request_set_referrer { id `ResourceRequest', id `AweString' } -> `()' #}
{#fun unsafe awe_resource_request_get_extra_headers { id `ResourceRequest' } -> `AweString' id #}
{#fun unsafe awe_resource_request_set_extra_headers { id `ResourceRequest', id `AweString' } -> `()' #}
{#fun unsafe awe_resource_request_append_extra_header { id `ResourceRequest', id `AweString', id `AweString' } -> `()' #}
{#fun unsafe awe_resource_request_get_num_upload_elements { id `ResourceRequest' } -> `Int' fromIntegral #}
--{#fun unsafe awe_resource_request_get_upload_element { id `ResourceRequest', size_t idx } -> `UploadElement' id #}
{#fun unsafe awe_resource_request_clear_upload_elements { id `ResourceRequest' } -> `()' #}
{#fun unsafe awe_resource_request_append_upload_file_path { id `ResourceRequest', id `AweString' } -> `()' #}
{#fun unsafe awe_resource_request_append_upload_bytes { id `ResourceRequest', id `AweString' } -> `()' #}

{------------------------
 - Upload Element       -
 ------------------------}

{#fun unsafe awe_upload_element_is_file_path { id `UploadElement' } -> `Bool' #}
{#fun unsafe awe_upload_element_is_bytes { id `UploadElement' } -> `Bool' #}
{#fun unsafe awe_upload_element_get_bytes { id `UploadElement' } -> `AweString' id #}
{#fun unsafe awe_upload_element_get_file_path { id `UploadElement' } -> `AweString' id #}

{------------------------
 - History Query Result -
 ------------------------}

{#fun unsafe awe_history_query_result_destroy { id `HistoryQueryResult' } -> `()' #}
{#fun unsafe awe_history_query_result_get_size { id `HistoryQueryResult' } -> `Int' fromIntegral #}
{#fun unsafe awe_history_query_result_get_entry_at_index { id `HistoryQueryResult', fromIntegral `Int' } -> `HistoryEntry' id #}

{------------------------
 - History Entry        -
 ------------------------}

{#fun unsafe awe_history_entry_destroy { id `HistoryEntry' } -> `()' #}
{#fun unsafe awe_history_entry_get_url { id `HistoryEntry' } -> `AweString' id #}
{#fun unsafe awe_history_entry_get_title { id `HistoryEntry' } -> `AweString' id #}
{#fun unsafe awe_history_entry_get_visit_time { id `HistoryEntry' } -> `Double' #}
{#fun unsafe awe_history_entry_get_visit_count { id `HistoryEntry' } -> `Int' #}

