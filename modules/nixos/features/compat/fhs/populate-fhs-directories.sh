# shellcheck shell=bash
# Populates the "/bin" and "/usr/bin" directories with the links in the
# directory named by the first argument. A link whose name that directory
# lacks is one that an earlier configuration placed, so the program removes
# it.

links_directory="$1"
for directory in /bin /usr/bin; do
  mkdir --parents "${directory}"
  for link in "${directory}"/*; do
    if [ -L "${link}" ] && ! [ -e "${links_directory}/${link##*/}" ]; then
      rm "${link}"
    fi
  done
  for entry in "${links_directory}"/*; do
    ln --symbolic --force --no-dereference "$(readlink "${entry}")" "${directory}/${entry##*/}"
  done
done
