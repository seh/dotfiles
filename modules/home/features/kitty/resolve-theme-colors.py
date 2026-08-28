# Print every color setting that a directory of kitty themes states,
# with kitty's own answer for what each one means.
#
# Kitty accepts more forms than "#rrggbb": a color name from a table of
# several hundred, three hexadecimal digits, twelve of them, and
# "rgb:rr/gg/bb". Reading them in the shell would mean keeping a second
# copy of that table and of kitty's parsing rules, so this asks kitty.
#
# Each line printed holds the theme's name, the setting, the value the
# theme states, and the red, green and blue that kitty reads it as,
# separated by tabs. A value kitty does not accept gets an empty last
# field, which is how the composing program learns to say so on that
# theme's page.
#
# The "kitty +launch" entry point runs this file with kitty's own
# modules importable. It runs while Nix builds the pages, never while
# the picker is open.

import pathlib
import sys

from kitty.rgb import to_color

# A theme file states many settings; these are the ones the preview
# draws with. A frozenset because the only question asked of it is
# whether a setting is among them.
COLOR_SETTINGS = frozenset({'background', 'foreground', *(f'color{index}' for index in range(16))})


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit('usage: resolve-theme-colors.py <directory of themes>')
    for path in sorted(pathlib.Path(sys.argv[1]).glob('*.conf')):
        for line in path.read_text(errors='replace').splitlines():
            fields = line.split(None, 2)
            if len(fields) < 2 or fields[0] not in COLOR_SETTINGS:
                continue
            color = to_color(fields[1])
            read_as = '' if color is None else f'{color.red};{color.green};{color.blue}'
            print(f'{path.stem}\t{fields[0]}\t{fields[1]}\t{read_as}')


main()
