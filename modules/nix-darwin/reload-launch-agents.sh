# shellcheck shell=bash
#
# Reload the named per-user LaunchAgents so a changed plist takes
# effect, using the modern "launchctl bootout"/"bootstrap" verbs. Run
# as root during activation, it re-enters the user's GUI domain with
# "launchctl asuser", so it takes effect only while that user has an
# active login (Aqua) session; with no such session it exits quietly
# without touching anything. Every external command is a built-in
# macOS tool.
#
# Each argument after the flags is a launchd "Label". nix-darwin
# gives every user agent's plist file the name of its "Label", so
# the plist sits at "~/Library/LaunchAgents/<Label>.plist":
# "bootout" targets the Label and "bootstrap" loads that file.
#
# Usage: reload-launch-agents --user <name> <label>...

user=

while (($# > 0)); do
  case "${1}" in
  --user)
    if (($# < 2)); then
      echo 'the --user flag requires a value' >&2
      exit 2
    fi
    user="${2}"
    shift 2
    ;;
  --*)
    echo "unrecognized flag: ${1}" >&2
    exit 2
    ;;
  *)
    break
    ;;
  esac
done

if [ -z "${user}" ]; then
  echo 'the --user flag is required' >&2
  exit 2
fi

if ! uid="$(/usr/bin/id -u -- "${user}")"; then
  echo "could not resolve a UID for the \"${user}\" user" >&2
  exit 1
fi

# The "bootout"/"bootstrap" verbs act on the user's GUI (Aqua) domain,
# which exists only while that user is logged in at the console. With
# no such session there is nothing loaded to reload, so exit quietly
# rather than emit a spurious error for every agent (e.g. over SSH).
if ! /bin/launchctl asuser "${uid}" /bin/launchctl print "gui/${uid}" >/dev/null 2>&1; then
  exit 0
fi

for label in "$@"; do
  plist="/Users/${user}/Library/LaunchAgents/${label}.plist"
  if [ ! -f "${plist}" ]; then
    continue
  fi

  # Tear the running job down so "bootstrap" installs the rewritten
  # plist. A not-currently-loaded job makes "bootout" fail harmlessly,
  # so capture its output and surface only a genuine teardown error.
  if ! bootout_error="$(/bin/launchctl asuser "${uid}" /usr/bin/sudo --user="${user}" -- \
    /bin/launchctl bootout "gui/${uid}/${label}" 2>&1)"; then
    case "${bootout_error}" in
    *'No such process'* | *'Could not find'*) : ;;
    *) echo "booting out the ${label} LaunchAgent: ${bootout_error}" >&2 ;;
    esac
  fi

  # "bootout" can lag behind its own return, so retry "bootstrap" a few
  # times to ride out that teardown race before giving up.
  attempt=0
  until /bin/launchctl asuser "${uid}" /usr/bin/sudo --user="${user}" -- \
    /bin/launchctl bootstrap "gui/${uid}" "${plist}"; do
    attempt=$((attempt + 1))
    if ((attempt >= 3)); then
      echo "could not reload the ${label} LaunchAgent" >&2
      break
    fi
    /bin/sleep 1
  done
done
