#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "TestPlugin_stub.h"
extern void __stginit_TestPlugin(void);
#endif
#include <stdio.h>
#include <webkit2/webkit-web-extension.h>

#include <HsFFI.h>

static void my_enter(void) __attribute__((constructor));
static void my_enter(void)
{
  static char *argv[] = { "libtest.so", 0 }, **argv_ = argv;
  static int argc = 1;
  hs_init(&argc, &argv_);
}


G_MODULE_EXPORT void
webkit_web_extension_initialize_with_user_data(WebKitWebExtension *extension,
                                               GVariant           *user_data)
{
#ifdef __GLASGOW_HASKELL__e
    hs_add_root(__stginit_TestPlugin);
#endif

    g_signal_connect (extension, "page-created",
                      G_CALLBACK (pageCreated_hs),
                      NULL);
}
