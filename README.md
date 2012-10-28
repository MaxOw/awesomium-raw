# Raw Awesomium Bindings #

Raw bindings to [Awesomium](http://www.awesomium.com)(1.6.5) for
Haskell. For more user friendly, higher-level bindings look
[here](https://github.com/MaxOw/awesomium).

You will need Awesomium library installed on your system, ideally in
`/usr/lib/awesomium-1.6.5/`. Otherwise you will have to change the
`extra-lib-dirs:` in `.cabal` file.

## TODO ##
1. Fix possible leak in WebkeyboardEvent poke implementation
2. Add bindings to the fallowing:
    * awe_is_child_process
    * awe_child_process_main
    * awe_webview_inject_keyboard_event_win
    * awe_renderbuffer_copy_to
    * awe_renderbuffer_copy_to_float 
    * awe_webview_set_callback_resource_request 
    * awe_webview_set_callback_resource_response 
    * awe_resource_response_create 
