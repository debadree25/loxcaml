#!/bin/sh
#
# setup.sh — bootstrap a local OCaml dev environment for loxcaml.
#
# Works on Linux and macOS. Idempotent: re-running is safe.
#
# What it does:
#   1. Installs OS prerequisites (gcc, make, curl, unzip, and on Linux bubblewrap)
#      using whatever package manager is present.
#   2. Installs opam (skipped if already present).
#   3. Initialises opam.
#      On Linux it first tests whether bubblewrap can actually create a
#      sandbox — bwrap commonly fails in Docker, WSL, and other environments
#      where unprivileged user namespaces are disabled even when the
#      package is installed. If that test fails, we fall back to
#      `opam init --disable-sandboxing` instead of letting opam break
#      mid-build.
#   4. Creates a project-local opam switch in ./_opam pinned to OCaml 5.2.0.
#   5. Installs project dependencies via `opam install . --deps-only --with-test`.
#   6. Runs `dune build` to verify.
#
# Override the OCaml version with: OCAML_VERSION=5.1.1 ./setup.sh

set -eu

OCAML_VERSION="${OCAML_VERSION:-5.2.0}"

log()  { printf '==> %s\n' "$*"; }
warn() { printf '!!! %s\n' "$*" >&2; }
die()  { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

# ---------- OS / package manager detection ----------

detect_os() {
  case "$(uname -s)" in
    Linux*)  echo linux ;;
    Darwin*) echo macos ;;
    *)       die "unsupported OS: $(uname -s) (this script supports Linux and macOS)" ;;
  esac
}

detect_pkg_manager() {
  if   command -v apt-get >/dev/null 2>&1; then echo apt
  elif command -v dnf     >/dev/null 2>&1; then echo dnf
  elif command -v yum     >/dev/null 2>&1; then echo yum
  elif command -v pacman  >/dev/null 2>&1; then echo pacman
  elif command -v zypper  >/dev/null 2>&1; then echo zypper
  elif command -v apk     >/dev/null 2>&1; then echo apk
  elif command -v brew    >/dev/null 2>&1; then echo brew
  else                                           echo unknown
  fi
}

sudo_or_not() {
  if [ "$(id -u)" = "0" ]; then echo ""; else echo "sudo"; fi
}

# ---------- system prerequisites ----------

install_prereqs() {
  os="$1"
  pkg="$(detect_pkg_manager)"
  SUDO="$(sudo_or_not)"

  if [ "$os" = "linux" ]; then
    log "installing Linux prerequisites via $pkg..."
    case "$pkg" in
      apt)
        $SUDO apt-get update
        $SUDO apt-get install -y gcc build-essential curl unzip bubblewrap m4 git
        ;;
      dnf|yum)
        $SUDO "$pkg" install -y gcc make curl unzip bubblewrap m4 git patch diffutils
        ;;
      pacman)
        $SUDO pacman -Sy --needed --noconfirm gcc make curl unzip bubblewrap m4 git patch diffutils
        ;;
      zypper)
        $SUDO zypper install -y gcc make curl unzip bubblewrap m4 git patch diffutils
        ;;
      apk)
        $SUDO apk add --no-cache gcc musl-dev make curl unzip bubblewrap m4 git patch
        ;;
      unknown)
        warn "no known package manager found"
        warn "please install manually: gcc, make, curl, unzip, bubblewrap, m4, git, patch"
        ;;
    esac
  elif [ "$os" = "macos" ]; then
    # On macOS, gcc/curl/unzip ship with Xcode Command Line Tools.
    # bubblewrap is Linux-only; opam uses sandbox-exec instead.
    if ! xcode-select -p >/dev/null 2>&1; then
      log "Xcode Command Line Tools not found — triggering installer..."
      xcode-select --install || true
      warn "complete the Xcode CLI installer dialog, then re-run this script"
      exit 1
    fi
    if [ "$pkg" = "brew" ]; then
      log "Homebrew detected; will use it to install opam"
    else
      log "Homebrew not found; will install opam via the upstream script"
    fi
  fi
}

# ---------- bubblewrap viability check ----------
#
# Returns 0 if bwrap is installed AND can actually create a sandbox.
# A non-functional bwrap is the classic opam-on-Linux gotcha: the binary
# exists but opam-built packages fail with cryptic errors. We rather skip
# sandboxing entirely than let that happen.
bwrap_works() {
  command -v bwrap >/dev/null 2>&1 || return 1
  bwrap --ro-bind / / --proc /proc --dev /dev /bin/true >/dev/null 2>&1
}

# ---------- opam install ----------

install_opam() {
  os="$1"
  if command -v opam >/dev/null 2>&1; then
    log "opam already installed: $(opam --version)"
    return
  fi
  if [ "$os" = "macos" ] && command -v brew >/dev/null 2>&1; then
    log "installing opam via Homebrew..."
    brew install opam
  else
    log "installing opam via the upstream script..."
    bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"
  fi
}

# ---------- main ----------

main() {
  OS="$(detect_os)"
  log "detected OS: $OS"

  install_prereqs "$OS"
  install_opam    "$OS"

  # opam init
  INIT_FLAGS="--no-setup --bare -y"
  if [ "$OS" = "linux" ]; then
    if bwrap_works; then
      log "bubblewrap is functional — opam will use sandboxing"
    else
      warn "bubblewrap is missing or non-functional"
      warn "(common in Docker, WSL, or kernels with unprivileged user namespaces disabled)"
      warn "falling back to: opam init --disable-sandboxing"
      INIT_FLAGS="$INIT_FLAGS --disable-sandboxing"
    fi
  fi

  if [ ! -d "$HOME/.opam" ]; then
    log "initialising opam ($INIT_FLAGS)..."
    # shellcheck disable=SC2086
    opam init $INIT_FLAGS
  else
    log "~/.opam already exists — skipping opam init"
  fi

  # local switch
  if [ ! -d "_opam" ]; then
    log "creating local opam switch with OCaml $OCAML_VERSION (this takes ~15 min)..."
    opam switch create . "ocaml-base-compiler.$OCAML_VERSION" --no-install -y
  else
    log "_opam/ already exists — skipping switch creation"
  fi

  # activate switch for the rest of this script
  eval "$(opam env --switch=. --set-switch)"

  # project deps
  if [ -f loxcaml.opam ]; then
    log "installing project dependencies (dune, lwt, alcotest)..."
    opam install . --deps-only --with-test -y
  else
    # fallback if the dune-generated opam file isn't present yet
    log "loxcaml.opam not found; installing core deps directly..."
    opam install dune lwt alcotest -y
  fi

  # verify
  log "verifying with dune build..."
  dune build

  printf '\n'
  log "setup complete."
  printf '\n'
  printf 'To activate this switch in a new shell:\n'
  printf '  eval $(opam env --switch=%s --set-switch)\n' "$(pwd)"
  printf '\n'
  printf 'Next:\n'
  printf '  dune build        # build loxcaml\n'
  printf '  dune test         # run the alcotest suite\n'
  printf '  ./bench/run.sh    # run benchmarks\n'
}

main "$@"
