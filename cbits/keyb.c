#include <stdlib.h>
#include "keyb.h"

void awe_webview_get_dirty_bounds_wrapper(awe_webview* wv, awe_rect* r) {
    *r = awe_webview_get_dirty_bounds(wv);
}

void awe_webview_inject_keyboard_event_wrapper (awe_webview* wv, awe_webkeyboardevent* ev) {
    awe_webview_inject_keyboard_event(wv, *ev);
}
