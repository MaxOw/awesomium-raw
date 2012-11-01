#ifndef KEYB_H
#define KEYB_H
#define bool uint
#include <stdlib.h>
#include "awesomium_capi.h"

void awe_webview_get_dirty_bounds_wrapper(awe_webview*, awe_rect*);
void awe_webview_inject_keyboard_event_wrapper(awe_webview*, awe_webkeyboardevent*);

#endif
