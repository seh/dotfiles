# shellcheck shell=bash
#
# Prepare the keychain that the "granted" tool keeps its secrets in,
# so that macOS asks for that keychain's password at the chosen
# interval instead of leaving it unlocked for the whole session.
#
# macOS gives any new keychain a five-minute lock timeout, and both
# reading and changing that timeout need the keychain unlocked, which
# asks the owner for its password. Creating the keychain here is
# therefore the one moment that sets the intended interval quietly,
# and it is why this runs rather than letting the "granted" tool
# create the keychain on its first write.
#
# The state file contains the interval last applied, standing in for
# reading the keychain itself. The "security show-keychain-info"
# command does report that interval, as the "timeout" field that "-t"
# sets, but it unlocks the keychain before reporting, so consulting it
# would raise a dialog on every run that found the keychain locked. A
# missing or damaged state file reads as zero, which matches no
# interval, so the settings get applied again.

function usage() {
  printf 'usage: %s [-h] -i seconds -k keychain_path -s state_path\n' "$(basename "${0}")" >&2
  exit 2
}

interval=
keychain=
state=

function parse_args() {
  while getopts hi:k:s: name; do
    case "${name}" in
      h) usage ;;
      i) interval="${OPTARG}" ;;
      k) keychain="${OPTARG}" ;;
      s) state="${OPTARG}" ;;
      ?) usage ;;
    esac
  done
  case "${interval}" in
    '' | *[!0-9]*)
      printf '%s: interval must be a count of seconds\n' "$(basename "${0}")" >&2
      exit 2
      ;;
  esac
  if [ -z "${keychain}" ]; then
    printf '%s: keychain file path must not be empty\n' "$(basename "${0}")" >&2
    exit 2
  fi
  if [ -z "${state}" ]; then
    printf '%s: state file path must not be empty\n' "$(basename "${0}")" >&2
    exit 2
  fi
}

parse_args "$@"
shift $((OPTIND - 1))

if (($# > 0)); then
  printf '%s: unexpected arguments\n' "$(basename "${0}")" >&2
  usage
fi

if [ ! -f "${keychain}" ]; then
  /usr/bin/security create-keychain -P "${keychain}"
  recorded=0
else
  recorded=0
  if [ -r "${state}" ]; then
    contents=$(cat "${state}")
    case "${contents}" in
      '' | *[!0-9]*) ;;
      *) recorded=${contents} ;;
    esac
  fi
fi

if ((recorded != interval)); then
  # The "-l" flag locks the keychain on sleep, which a fixed interval
  # misses, and "-u -t" sets the idle timeout.
  /usr/bin/security set-keychain-settings -l -u -t "${interval}" "${keychain}"
  mkdir -p "$(dirname "${state}")"
  echo "${interval}" >"${state}"
fi
