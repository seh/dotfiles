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
# Usage: reload-launch-agents [-h] -u user label...

function usage() {
  printf 'usage: %s [-h] -u user label...\n' "$(basename "${0}")" >&2
  exit 2
}

user=

function parse_args() {
  while getopts hu: name; do
    case "${name}" in
    h) usage ;;
    u) user="${OPTARG}" ;;
    ?) usage ;;
    esac
  done
  if [ -z "${user}" ]; then
    printf '%s: user name must not be empty\n' "$(basename "${0}")" >&2
    exit 2
  fi
}

parse_args "$@"
shift $((OPTIND - 1))

if ! uid="$(/usr/bin/id -u -- "${user}")"; then
  echo "could not resolve a UID for the \"${user}\" user" >&2
  exit 1
fi

# "bootout" and "bootstrap" act on the user's GUI (Aqua) domain, which
# exists only while that user has a console session. Exit quietly
# when that session is absent, rather than print an error for every
# agent, as would happen over SSH.
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

  # "bootout" can return before the job has finished shutting down, so
  # retry "bootstrap" a few times before reporting failure.
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
