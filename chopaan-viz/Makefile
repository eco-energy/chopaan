# Makefile for chopaan-viz (dear-imgui grid renderer + FFI Hopfield kernel).
#
#   make          # build the binary
#   make run      # build if needed, then launch the window
#   make clean    # remove dist-newstyle
#
# The renderer's OpenGL3/SDL2 backends need system libs (SDL2, libGL, GLEW,
# GLU) and the X11 stack on the library path both to build and to run. The
# nix store paths below are pinned to this environment; on another machine,
# override them (e.g. `make run SDL2=/usr GL=/usr`) or use the nix-shell
# one-liner in README.md.
#
# NOTE: running needs a display. This is headless in the container, so
# `make run` only launches a window where $DISPLAY points at an X server.

# --- toolchain -------------------------------------------------------------
GHC     ?= /nix/store/am43aci0paslfnq5c0l42553h8dsycad-ghc-9.0.1
CABALBIN?= /nix/store/jb994ri3s5d5pmrhxzq1q8xgzrfpg7dx-cabal-install-3.6.2.0/bin
PKGCONF ?= /nix/store/131vdr1hvmmk8icrdfppa4ksbg3jdjxr-pkg-config-wrapper-0.29.2/bin
CCBIN   ?= /nix/store/myjrqkb583ak29f3r5wmxrazy33i90pi-gcc-wrapper-10.3.0/bin

# --- system libraries (runtime + build) ------------------------------------
SDL2    ?= /nix/store/5ibq3yrg14ww961k711lc2a2swq6mk74-SDL2-2.0.14
SDL2DEV ?= /nix/store/1nrg5qydrcsa39nzl6rd6dqb2i1gdcx4-SDL2-2.0.14-dev
GL      ?= /nix/store/zhlgnwhz8qyi0iv196mpmxnj5vcc5chn-libGL-1.3.4
GLDEV   ?= /nix/store/0qblrvs994a24nrnci41pxj3a8qd7q91-libGL-1.3.4-dev
GLEW    ?= /nix/store/g3sj2w93s6z4zlymnyy4z7hscs687hfr-glew-2.2.0-bin
GLEWDEV ?= /nix/store/5mmqxkpbkdkirxpd236p4crpahaj94wl-glew-2.2.0-dev
GLU     ?= /nix/store/5x44mjbj4zz1ig88yzrjvwgsp45fifkq-glu-9.0.2
GLUDEV  ?= /nix/store/60kzh2xqcxb6340nlvrlyn2gbv8dhy28-glu-9.0.2-dev

# X11 stack pulled in by SDL2/GLEW at link + run time.
X11LIBS := \
  /nix/store/6d86c2cl648dwa1w8dcfdbrk4d2x6jg4-libX11-1.7.2/lib \
  /nix/store/knl9961fyiaigwx5q32szr0h5v86x8f8-libXext-1.3.4/lib \
  /nix/store/dcddxwl9ry1chkbcxd5qkkbmbgs2szf4-libXcursor-1.2.0/lib \
  /nix/store/rsmfdplxf9jbajwr8q1ifigjlg8rljs0-libXi-1.8/lib \
  /nix/store/ydkf6mar1p59j256r4nhjw1qm6clq8h6-libXrandr-1.5.2/lib \
  /nix/store/5pcn565fc40xbsny15y070kil62wj6ly-libXfixes-6.0.0/lib \
  /nix/store/yaaj7z6v3w9mff0ncgvy0dajj8z53pzn-libXScrnSaver-1.2.3/lib \
  /nix/store/0w1mqddp5nnkvmishrnn13p2y6bzvqj6-libXxf86vm-1.1.4/lib \
  /nix/store/d02wf5yiqbfz1x9ppz3lgv0gnckik6mz-libxcb-1.14/lib \
  /nix/store/9w8b01n8xn98k7d3zb9jb8a9i4a9lr3r-libXrender-0.9.10/lib

# X11 headers needed at build time.
X11INC := \
  /nix/store/zzk9h7s3nii3wdabv7g9h26fqcha2scn-libX11-1.7.2-dev/include \
  /nix/store/icaymmmhzmyrfnf4vk2ymfhzv0f18b1y-xorgproto-2021.5/include \
  /nix/store/wgf8wcnxhk0486mgd0il0zi0sab4gfw2-libxcb-1.14-dev/include

empty :=
space := $(empty) $(empty)
LIBDIRS := $(subst $(space),:,$(X11LIBS)) :$(GL)/lib:$(GLEW)/lib:$(GLU)/lib:$(SDL2)/lib
INCDIRS := $(subst $(space),:,$(X11INC)) :$(SDL2DEV)/include:$(GLDEV)/include:$(GLEWDEV)/include

# --- cabal invocation ------------------------------------------------------
CABAL       := $(CABALBIN)/cabal
CABAL_FLAGS := --index-state=2023-06-01T00:00:00Z \
               --constraint="dear-imgui <2.2" --constraint="sdl2 <2.5.4"
export CABAL_DIR := /tmp/cabal-viz
export HOME      := /tmp/cabal-viz

# Prepend the toolchain + expose libs/headers to ghc, gcc and pkg-config.
export PATH             := $(GHC)/bin:$(CABALBIN):$(PKGCONF):$(CCBIN):$(PATH)
export PKG_CONFIG_PATH  := $(SDL2DEV)/lib/pkgconfig:$(GLDEV)/lib/pkgconfig:$(GLEWDEV)/lib/pkgconfig:$(GLUDEV)/lib/pkgconfig
export LIBRARY_PATH     := $(LIBDIRS)
export LD_LIBRARY_PATH  := $(LIBDIRS)
export C_INCLUDE_PATH   := $(INCDIRS)
export CPLUS_INCLUDE_PATH := $(INCDIRS)

BIN := $(shell find dist-newstyle -type f -name chopaan-viz -perm -u+x 2>/dev/null | head -1)

.PHONY: all build run clean

all: build

build:
	mkdir -p $(CABAL_DIR)
	$(CABAL) build $(CABAL_FLAGS) -w $(GHC)/bin/ghc \
	  --extra-lib-dirs="$(SDL2)/lib" --extra-lib-dirs="$(GL)/lib" --extra-lib-dirs="$(GLEW)/lib" \
	  --extra-include-dirs="$(SDL2DEV)/include" --extra-include-dirs="$(GLDEV)/include" --extra-include-dirs="$(GLEWDEV)/include" \
	  chopaan-viz

# Launch the window. Uses `cabal run` so it builds first if needed; the
# library path is exported above so the linked SDL2/GL/GLEW/GLU/X11 libs
# resolve at runtime.
run:
	@if [ -z "$$DISPLAY" ]; then \
	  echo ">> No \$$DISPLAY set — this needs an X server. Launching anyway; it will exit if none is found."; \
	fi
	mkdir -p $(CABAL_DIR)
	$(CABAL) run $(CABAL_FLAGS) -w $(GHC)/bin/ghc \
	  --extra-lib-dirs="$(SDL2)/lib" --extra-lib-dirs="$(GL)/lib" --extra-lib-dirs="$(GLEW)/lib" \
	  --extra-include-dirs="$(SDL2DEV)/include" --extra-include-dirs="$(GLDEV)/include" --extra-include-dirs="$(GLEWDEV)/include" \
	  chopaan-viz

clean:
	rm -rf dist-newstyle
