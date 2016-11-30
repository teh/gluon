#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "TestPlugin_stub.h"
extern void __stginit_TestPlugin(void);
#endif
#include <stdio.h>
#include <webkit2/webkit-web-extension.h>

#include <HsFFI.h>
#include <Rts.h>

static void my_exit(void) __attribute__((destructor));
static void my_exit(void)
{
  hs_exit(); // required to write eventlog data to disk
}

G_MODULE_EXPORT void
webkit_web_extension_initialize_with_user_data(WebKitWebExtension *extension,
                                               GVariant           *user_data)
{
  static char *argv[] = { "libtest.so", 0 }, **argv_ = argv;
  static int argc = 1;
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  conf.rts_opts = "-l";
  hs_init_ghc(&argc, &argv_, conf);

  hs_add_root(__stginit_TestPlugin);

  g_signal_connect (extension, "page-created",
                    G_CALLBACK (pageCreated_hs),
                    NULL);
}
