# stripped down build.mk that fits our needs
# if you want to tweak them, see mk/build.mk.sample in the dist tarball
SRC_HC_OPTS        = -O -H64m
GhcStage1HcOpts    = -O -fasm
GhcStage2HcOpts    = -O2 -fasm
GhcHcOpts          = -Rghc-timing
GhcLibHcOpts       = -O2 -XGenerics
GhcLibWays         = v p
SplitObjs          = NO
HADDOCK_DOCS       = NO
BUILD_DOCBOOK_HTML = NO
BUILD_DOCBOOK_PS   = NO
BUILD_DOCBOOK_PDF  = NO
HSCOLOUR_SRCS      = NO
