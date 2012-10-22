{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.UI.Awesomium.Raw where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

{#context prefix = "awe"#}

#define bool uint
#include "awesomium_capi.h"

type WChar16 = {#type wchar16#}
type Int64 = {#type int64#}

-- | WebView instance
data DWebView
{#pointer *webview as WebView -> DWebView #}
-- | JSValue instance
{#pointer *jsvalue as JSValue #}
-- | JSArray instance
{#pointer *jsarray as JSArray #}
-- | JSObject instance
{#pointer *jsobject as JSObject #}
-- | RenderBuffer instance, owned by the WebView
{#pointer *renderbuffer as RenderBuffer #}
-- | HeaderDefinition instance
{#pointer *header_definition as HeaderDefinition #}
-- | ResourceResponse instance
{#pointer *resource_response as ResourceResponse #}
-- | ResourceRequest instance
{#pointer *resource_request as ResourceRequest #}
-- | UploadElement instance
{#pointer *upload_element as UploadElement #}
-- | String instance
{#pointer *awe_string as AweString #}
-- | HistoryQueryResult instance
{#pointer *history_query_result as HistoryQueryResult #}
-- | HistoryEntry instance
{#pointer *history_entry as HistoryEntry #}

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
    bool is_system_key;
} awe_webkeyboardevent;

typedef struct _awe_rect
{
    int x, y, width, height;
} awe_rect;

#ifdef _WIN32
_OSMExport bool awe_is_child_process(HINSTANCE hInstance);
_OSMExport int awe_child_process_main(HINSTANCE hInstance);
#else
_OSMExport bool awe_is_child_process(int argc, char** argv);
_OSMExport int awe_child_process_main(int argc, char** argv);
#endif

_OSMExport const awe_string* awe_string_empty();
_OSMExport awe_string* awe_string_create_from_ascii(const char* str, size_t len);
_OSMExport awe_string* awe_string_create_from_wide(const wchar_t* str, size_t len);
_OSMExport awe_string* awe_string_create_from_utf8(const char* str, size_t len);
_OSMExport awe_string* awe_string_create_from_utf16(const wchar16* str, size_t len);
_OSMExport void awe_string_destroy(awe_string* str);
_OSMExport size_t awe_string_get_length(const awe_string* str);
_OSMExport const wchar16* awe_string_get_utf16(const awe_string* str);
_OSMExport int awe_string_to_wide(const awe_string* str, wchar_t* dest, size_t len);
_OSMExport int awe_string_to_utf8(const awe_string* str, char* dest, size_t len);

{-**********************
 - Web Core Functions  *
 -*********************-}

_OSMExport void awe_webcore_initialize(bool enable_plugins,
                                       bool enable_javascript,
                                       bool enable_databases,
                                       const awe_string* package_path,
                                       const awe_string* locale_path,
                                       const awe_string* user_data_path,
                                       const awe_string* plugin_path,
                                       const awe_string* log_path,
                                       awe_loglevel log_level,
                                       bool force_single_process,
                                       const awe_string* child_process_path,
                                       bool enable_auto_detect_encoding,
                                       const awe_string* accept_language_override,
                                       const awe_string* default_charset_override,
                                       const awe_string* user_agent_override,
                                       const awe_string* proxy_server,
                                       const awe_string* proxy_config_script,
                                       const awe_string* auth_server_whitelist,
                                       bool save_cache_and_cookies,
                                       int max_cache_size,
                                       bool disable_same_origin_policy,
                                       bool disable_win_message_pump,
                                       const awe_string* custom_css);

-}

{#fun unsafe awe_webcore_initialize_default { } -> `()' #}
{#fun unsafe awe_webcore_shutdown { } -> `()' #}

-- {#fun unsafe awe_webcore_set_base_directory { const awe_string* base_dir_path } -> `()' #}
-- {#fun unsafe awe_webcore_create_webview { `Int', `Int', bool view_source } -> awe_webview* #}
-- {#fun unsafe awe_webcore_set_custom_response_page { `Int', const awe_string* file_path } -> `()' #}
{#fun unsafe awe_webcore_update { } -> `()' #}
-- {#fun unsafe awe_webcore_get_base_directory { } -> const awe_string* #}
-- {#fun unsafe awe_webcore_are_plugins_enabled { } -> bool #}
{#fun unsafe awe_webcore_clear_cache { } -> `()' #}
{#fun unsafe awe_webcore_clear_cookies { } -> `()' #}
-- {#fun unsafe awe_webcore_set_cookie { const awe_string* url, const awe_string* cookie_string, bool is_http_only, bool force_session_cookie } -> `()' #}
-- {#fun unsafe awe_webcore_get_cookies { const awe_string* url, bool exclude_http_only } -> const awe_string* #}
-- {#fun unsafe awe_webcore_delete_cookie { const awe_string* url, const awe_string* cookie_name } -> `()' #}
-- {#fun unsafe awe_webcore_set_suppress_printer_dialog { bool suppress } -> `()' #}
-- {#fun unsafe awe_webcore_query_history { const awe_string* full_text_query, `Int', `Int' } -> awe_history_query_result* #}

{-

{-**********************
 - Web View Functions  *
 -*********************-}

_OSMExport void awe_webview_destroy(id `WebView');
_OSMExport void awe_webview_load_url(id `WebView', const awe_string* url, const awe_string* frame_name, const awe_string* username, const awe_string* password);
_OSMExport void awe_webview_load_html(id `WebView', const awe_string* html, const awe_string* frame_name);
_OSMExport void awe_webview_load_file(id `WebView', const awe_string* file, const awe_string* frame_name);
_OSMExport awe_string* awe_webview_get_url(id `WebView');
_OSMExport void awe_webview_go_to_history_offset(id `WebView', int offset);
_OSMExport int awe_webview_get_history_back_count(id `WebView');
_OSMExport int awe_webview_get_history_forward_count(id `WebView');
_OSMExport void awe_webview_stop(id `WebView');
_OSMExport void awe_webview_reload(id `WebView');
_OSMExport void awe_webview_execute_javascript(id `WebView', const awe_string* javascript, const awe_string* frame_name);
_OSMExport awe_jsvalue* awe_webview_execute_javascript_with_result( id `WebView', const awe_string* javascript, const awe_string* frame_name, int timeout_ms);
_OSMExport void awe_webview_call_javascript_function(id `WebView', const awe_string* object, const awe_string* function, const awe_jsarray* arguments, const awe_string* frame_name);
_OSMExport void awe_webview_create_object(id `WebView', const awe_string* object_name);
_OSMExport void awe_webview_destroy_object(id `WebView', const awe_string* object_name);
_OSMExport void awe_webview_set_object_property(id `WebView', const awe_string* object_name, const awe_string* property_name, const awe_jsvalue* value);
_OSMExport void awe_webview_set_object_callback(id `WebView', const awe_string* object_name, const awe_string* callback_name);
_OSMExport bool awe_webview_is_loading_page(id `WebView');
_OSMExport bool awe_webview_is_dirty(id `WebView');
_OSMExport awe_rect awe_webview_get_dirty_bounds(id `WebView');
_OSMExport const awe_renderbuffer* awe_webview_render(id `WebView');
_OSMExport void awe_webview_pause_rendering(id `WebView');
_OSMExport void awe_webview_resume_rendering(id `WebView');
_OSMExport void awe_webview_inject_mouse_move(id `WebView', int x, int y);
_OSMExport void awe_webview_inject_mouse_down(id `WebView', awe_mousebutton button);
_OSMExport void awe_webview_inject_mouse_up(id `WebView', awe_mousebutton button);
_OSMExport void awe_webview_inject_mouse_wheel(id `WebView', int scroll_amount_vert, int scroll_amount_horz);
_OSMExport void awe_webview_inject_keyboard_event(id `WebView', awe_webkeyboardevent key_event);

#ifdef _WIN32
_OSMExport void awe_webview_inject_keyboard_event_win(id `WebView', UINT msg, WPARAM wparam, LPARAM lparam);
#endif

_OSMExport void awe_webview_cut(id `WebView');
_OSMExport void awe_webview_copy(id `WebView');
_OSMExport void awe_webview_paste(id `WebView');
_OSMExport void awe_webview_select_all(id `WebView');
_OSMExport void awe_webview_copy_image_at(id `WebView', int x, int y);
_OSMExport void awe_webview_set_zoom(id `WebView', int zoom_percent);
_OSMExport void awe_webview_reset_zoom(id `WebView');
_OSMExport int awe_webview_get_zoom(id `WebView');
_OSMExport int awe_webview_get_zoom_for_host(id `WebView', const awe_string* host);
_OSMExport bool awe_webview_resize(id `WebView', int width, int height, bool wait_for_repaint, int repaint_timeout_ms);
_OSMExport bool awe_webview_is_resizing(id `WebView');
_OSMExport void awe_webview_unfocus(id `WebView');
_OSMExport void awe_webview_focus(id `WebView');
_OSMExport void awe_webview_set_transparent(id `WebView', bool is_transparent);
_OSMExport bool awe_webview_is_transparent(id `WebView');
_OSMExport void awe_webview_set_url_filtering_mode(id `WebView', awe_url_filtering_mode mode);
_OSMExport void awe_webview_add_url_filter(id `WebView', const awe_string* filter);
_OSMExport void awe_webview_clear_all_url_filters(id `WebView');
_OSMExport void awe_webview_set_header_definition(id `WebView', const awe_string* name, size_t num_fields, const awe_string** field_names, const awe_string** field_values);
_OSMExport void awe_webview_add_header_rewrite_rule(id `WebView', const awe_string* rule, const awe_string* name);
_OSMExport void awe_webview_remove_header_rewrite_rule(id `WebView', const awe_string* rule);
_OSMExport void awe_webview_remove_header_rewrite_rules_by_definition_name( id `WebView', const awe_string* name);
_OSMExport void awe_webview_choose_file(id `WebView', const awe_string* file_path);
_OSMExport void awe_webview_print(id `WebView');
_OSMExport void awe_webview_request_scroll_data(id `WebView', const awe_string* frame_name);
_OSMExport void awe_webview_find(id `WebView', int request_id, const awe_string* search_string, bool forward, bool case_sensitive, bool find_next);
_OSMExport void awe_webview_stop_find(id `WebView', bool clear_selection);
_OSMExport void awe_webview_translate_page(id `WebView', const awe_string* source_language, const awe_string* target_language);
_OSMExport void awe_webview_activate_ime(id `WebView', bool activate);
_OSMExport void awe_webview_set_ime_composition(id `WebView', const awe_string* input_string, int cursor_pos, int target_start, int target_end);
_OSMExport void awe_webview_confirm_ime_composition(id `WebView', const awe_string* input_string);
_OSMExport void awe_webview_cancel_ime_composition(id `WebView');
_OSMExport void awe_webview_login(id `WebView', int request_id, const awe_string* username, const awe_string* password);
_OSMExport void awe_webview_cancel_login(id `WebView', int request_id);
_OSMExport void awe_webview_close_javascript_dialog(id `WebView', int request_id, bool was_cancelled, const awe_string* prompt_text);
_OSMExport void awe_webview_set_callback_begin_navigation( id `WebView', void (*callback)(awe_webview* caller, const awe_string* url, const awe_string* frame_name));
_OSMExport void awe_webview_set_callback_begin_loading( id `WebView', void (*callback)(awe_webview* caller, const awe_string* url, const awe_string* frame_name, int status_code, const awe_string* mime_type));
_OSMExport void awe_webview_set_callback_finish_loading( id `WebView', void (*callback)(awe_webview* caller));
_OSMExport void awe_webview_set_callback_js_callback( id `WebView', void (*callback)(awe_webview* caller, const awe_string* object_name, const awe_string* callback_name, const awe_jsarray* arguments));
_OSMExport void awe_webview_set_callback_receive_title( id `WebView', void (*callback)(awe_webview* caller, const awe_string* title, const awe_string* frame_name));
_OSMExport void awe_webview_set_callback_change_tooltip( id `WebView', void (*callback)(awe_webview* caller, const awe_string* tooltip));
_OSMExport void awe_webview_set_callback_change_cursor( id `WebView', void (*callback)(awe_webview* caller, awe_cursor_type cursor));
_OSMExport void awe_webview_set_callback_change_keyboard_focus( id `WebView', void (*callback)(awe_webview* caller, bool is_focused));
_OSMExport void awe_webview_set_callback_change_target_url( id `WebView', void (*callback)(awe_webview* caller, const awe_string* url));
_OSMExport void awe_webview_set_callback_open_external_link( id `WebView', void (*callback)(awe_webview* caller, const awe_string* url, const awe_string* source));
_OSMExport void awe_webview_set_callback_request_download( id `WebView', void (*callback)(awe_webview* caller, const awe_string* download));
_OSMExport void awe_webview_set_callback_web_view_crashed( id `WebView', void (*callback)(awe_webview* caller));
_OSMExport void awe_webview_set_callback_plugin_crashed( id `WebView', void (*callback)(awe_webview* caller, const awe_string* plugin_name));
_OSMExport void awe_webview_set_callback_request_move( id `WebView', void (*callback)(awe_webview* caller, int x, int y));
_OSMExport void awe_webview_set_callback_get_page_contents( id `WebView', void (*callback)(awe_webview* caller, const awe_string* url, const awe_string* contents));
_OSMExport void awe_webview_set_callback_dom_ready( id `WebView', void (*callback)(awe_webview* caller));
_OSMExport void awe_webview_set_callback_request_file_chooser( id `WebView', void (*callback)(awe_webview* caller, bool select_multiple_files, const awe_string* title, const awe_string* default_path));
_OSMExport void awe_webview_set_callback_get_scroll_data( id `WebView', void (*callback)(awe_webview* caller, int contentWidth, int contentHeight, int preferredWidth, int scrollX, int scrollY));
_OSMExport void awe_webview_set_callback_js_console_message( id `WebView', void (*callback)(awe_webview* caller, const awe_string* message, int line_number, const awe_string* source));
_OSMExport void awe_webview_set_callback_get_find_results( id `WebView', void (*callback)(awe_webview* caller, int request_id, int num_matches, awe_rect selection, int cur_match, bool finalUpdate));
_OSMExport void awe_webview_set_callback_update_ime( id `WebView', void (*callback)(awe_webview* caller, awe_ime_state state, awe_rect caret_rect));
_OSMExport void awe_webview_set_callback_show_context_menu( id `WebView', void (*callback)(awe_webview* caller, int mouse_x, int mouse_y, awe_media_type type, int media_state, const awe_string* link_url, const awe_string* src_url, const awe_string* page_url, const awe_string* frame_url, const awe_string* selection_text, bool is_editable, int edit_flags));
_OSMExport void awe_webview_set_callback_request_login( id `WebView', void (*callback)(awe_webview* caller, int request_id, const awe_string* request_url, bool is_proxy, const awe_string* host_and_port, const awe_string* scheme, const awe_string* realm));
_OSMExport void awe_webview_set_callback_change_history( id `WebView', void (*callback)(awe_webview* caller, int back_count, int forward_count));
_OSMExport void awe_webview_set_callback_finish_resize( id `WebView', void (*callback)(awe_webview* caller, int width, int height));
_OSMExport void awe_webview_set_callback_show_javascript_dialog( id `WebView', void (*callback)(awe_webview* caller, int request_id, int dialog_flags, const awe_string* message, const awe_string* default_prompt, const awe_string* frame_url));

{-**********************
 - JS Value Functions  *
 -*********************-}

{#enum jsvalue_type as JSValueType {underscoreToCase}#}

_OSMExport awe_jsvalue* awe_jsvalue_create_null_value();
_OSMExport awe_jsvalue* awe_jsvalue_create_bool_value(bool value);
_OSMExport awe_jsvalue* awe_jsvalue_create_integer_value(int value);
_OSMExport awe_jsvalue* awe_jsvalue_create_double_value(double value);
_OSMExport awe_jsvalue* awe_jsvalue_create_string_value(const awe_string* value);
_OSMExport awe_jsvalue* awe_jsvalue_create_object_value(const awe_jsobject* value);
_OSMExport awe_jsvalue* awe_jsvalue_create_array_value(const awe_jsarray* value);
_OSMExport void awe_jsvalue_destroy(awe_jsvalue* jsvalue);
_OSMExport awe_jsvalue_type awe_jsvalue_get_type(const awe_jsvalue* jsvalue);
_OSMExport awe_string* awe_jsvalue_to_string(const awe_jsvalue* jsvalue);
_OSMExport int awe_jsvalue_to_integer(const awe_jsvalue* jsvalue);
_OSMExport double awe_jsvalue_to_double(const awe_jsvalue* jsvalue);
_OSMExport bool awe_jsvalue_to_boolean(const awe_jsvalue* jsvalue);
_OSMExport const awe_jsarray* awe_jsvalue_get_array(const awe_jsvalue* jsvalue);
_OSMExport const awe_jsobject* awe_jsvalue_get_object(const awe_jsvalue* jsvalue);
_OSMExport awe_jsarray* awe_jsarray_create(const awe_jsvalue** jsvalue_array, size_t length);
_OSMExport void awe_jsarray_destroy(awe_jsarray* jsarray);
_OSMExport size_t awe_jsarray_get_size(const awe_jsarray* jsarray);
_OSMExport const awe_jsvalue* awe_jsarray_get_element(const awe_jsarray* jsarray, size_t index);

{-****************************
 - JS Value Object Functions *
 -***************************-}

_OSMExport awe_jsobject* awe_jsobject_create();
_OSMExport void awe_jsobject_destroy(awe_jsobject* jsobject);
_OSMExport bool awe_jsobject_has_property(const awe_jsobject* jsobject, const awe_string* property_name);
_OSMExport const awe_jsvalue* awe_jsobject_get_property(const awe_jsobject* jsobject, const awe_string* property_name);
_OSMExport void awe_jsobject_set_property(awe_jsobject* jsobject, const awe_string* property_name, const awe_jsvalue* value);
_OSMExport size_t awe_jsobject_get_size(awe_jsobject* jsobject);
_OSMExport awe_jsarray* awe_jsobject_get_keys(awe_jsobject* jsobject);

{-**************************
 - Render Buffer Functions *
 -*************************-}

_OSMExport int awe_renderbuffer_get_width(const awe_renderbuffer* renderbuffer);
_OSMExport int awe_renderbuffer_get_height(const awe_renderbuffer* renderbuffer);
_OSMExport int awe_renderbuffer_get_rowspan(const awe_renderbuffer* renderbuffer);
_OSMExport const unsigned char* awe_renderbuffer_get_buffer( const awe_renderbuffer* renderbuffer);
_OSMExport void awe_renderbuffer_copy_to(const awe_renderbuffer* renderbuffer, unsigned char* dest_buffer, int dest_rowspan, int dest_depth, bool convert_to_rgba, bool flip_y);
_OSMExport void awe_renderbuffer_copy_to_float(const awe_renderbuffer* renderbuffer, float* dest_buffer);
_OSMExport bool awe_renderbuffer_save_to_png(const awe_renderbuffer* renderbuffer, const awe_string* file_path, bool preserve_transparency);
_OSMExport bool awe_renderbuffer_save_to_jpeg(const awe_renderbuffer* renderbuffer, const awe_string* file_path, int quality);
_OSMExport unsigned char awe_renderbuffer_get_alpha_at_point(const awe_renderbuffer* renderbuffer, int x, int y);
_OSMExport void awe_renderbuffer_flush_alpha(const awe_renderbuffer* renderbuffer);

{-***********************
 - Resource Interceptor *
 -**********************-}

_OSMExport void awe_webview_set_callback_resource_request( id `WebView', awe_resource_response* (*callback)( awe_webview* caller, awe_resource_request* request));
_OSMExport void awe_webview_set_callback_resource_response( id `WebView', void (*callback)( awe_webview* caller, const awe_string* url, int status_code, bool was_cached, int64 request_time_ms, int64 response_time_ms, int64 expected_content_size, const awe_string* mime_type));
_OSMExport awe_resource_response* awe_resource_response_create( size_t num_bytes, unsigned char* buffer, const awe_string* mime_type);
_OSMExport awe_resource_response* awe_resource_response_create_from_file( const awe_string* file_path);

{-***********************
 - Resource Request     *
 -**********************-}

_OSMExport void awe_resource_request_cancel(awe_resource_request* request);
_OSMExport awe_string* awe_resource_request_get_url(awe_resource_request* request);
_OSMExport awe_string* awe_resource_request_get_method(awe_resource_request* request);
_OSMExport void awe_resource_request_set_method(awe_resource_request* request, const awe_string* method);
_OSMExport awe_string* awe_resource_request_get_referrer(awe_resource_request* request);
_OSMExport void awe_resource_request_set_referrer(awe_resource_request* request, const awe_string* referrer);
_OSMExport awe_string* awe_resource_request_get_extra_headers(awe_resource_request* request);
_OSMExport void awe_resource_request_set_extra_headers(awe_resource_request* request, const awe_string* headers);
_OSMExport void awe_resource_request_append_extra_header(awe_resource_request* request, const awe_string* name, const awe_string* value);
_OSMExport size_t awe_resource_request_get_num_upload_elements(awe_resource_request* request);
_OSMExport const awe_upload_element* awe_resource_request_get_upload_element(awe_resource_request* request, size_t idx);
_OSMExport void awe_resource_request_clear_upload_elements(awe_resource_request* request);
_OSMExport void awe_resource_request_append_upload_file_path(awe_resource_request* request, const awe_string* file_path);
_OSMExport void awe_resource_request_append_upload_bytes(awe_resource_request* request, const awe_string* bytes);

{-***********************
 - Upload Element       *
 -**********************-}

_OSMExport bool awe_upload_element_is_file_path(const awe_upload_element* ele);
_OSMExport bool awe_upload_element_is_bytes(const awe_upload_element* ele);
_OSMExport awe_string* awe_upload_element_get_bytes(const awe_upload_element* ele);
_OSMExport awe_string* awe_upload_element_get_file_path(const awe_upload_element* ele);

{-***********************
 - History Query Result *
 -**********************-}

_OSMExport void awe_history_query_result_destroy(awe_history_query_result* res);
_OSMExport size_t awe_history_query_result_get_size(awe_history_query_result* res);
_OSMExport awe_history_entry* awe_history_query_result_get_entry_at_index(awe_history_query_result* res,

{-***********************
 - History Entry        *
 -**********************-}

_OSMExport void awe_history_entry_destroy(awe_history_entry* entry);
_OSMExport awe_string* awe_history_entry_get_url(awe_history_entry* entry);
_OSMExport awe_string* awe_history_entry_get_title(awe_history_entry* entry);
_OSMExport double awe_history_entry_get_visit_time(awe_history_entry* entry);
_OSMExport int awe_history_entry_get_visit_count(awe_history_entry* entry);

-}
