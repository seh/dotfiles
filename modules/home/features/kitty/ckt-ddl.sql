-- The state that the "ckt" program keeps: which versions of the
-- "kitty-themes" package this computer has seen, what each of them
-- offered, and which themes were chosen when.
--
-- The program applies this file when it finds a database whose
-- "user_version" is still zero, and sets that version itself once
-- these statements have succeeded, so applying them twice does
-- nothing the second time. The number it sets is stated there rather
-- than here, so that it has one home.
--
-- Nothing here answers, which is what lets the program submit the
-- whole file and expect a single acknowledgement. The journal mode
-- belongs with these statements but does answer, so the program sets
-- it separately and reads what it says.
--
-- Every column is declared NOT NULL. Six of the nine would be so
-- anyway, being the primary keys of WITHOUT ROWID tables, to which
-- SQLite applies it; a table with a rowid gets no such treatment, and
-- its primary key still accepts a null. Declaring it on every column
-- means the guarantee holds whatever anyone later decides about
-- storage.
--
-- Two columns hold moments, in different units. The "first_seen"
-- column below counts seconds, since it only ever orders one version
-- against another, while "occurred_at" counts milliseconds, which is
-- the resolution the decay arithmetic reads. Neither is ever compared
-- with the other.

BEGIN IMMEDIATE;

-- A version of the "kitty-themes" package, identified by the store
-- path of the file of theme names that the build produced from it.
-- The path is the version: its hash changes when its contents do.
CREATE TABLE IF NOT EXISTS catalog(
  path TEXT NOT NULL PRIMARY KEY
    CONSTRAINT catalog_path_lies_in_the_nix_store CHECK (path LIKE '/nix/store/%'),
  first_seen INTEGER NOT NULL
) WITHOUT ROWID;

-- Every theme this computer has been offered or has chosen. A theme
-- of a given name is taken to be the same theme in every version of
-- the package, which is why a choice points here and not at one
-- version's listing of it.
CREATE TABLE IF NOT EXISTS theme(
  name TEXT NOT NULL PRIMARY KEY
    CONSTRAINT theme_name_is_not_empty CHECK (length(name) > 0)
) WITHOUT ROWID;

-- What one version offered. A version never changes, so these rows
-- are written the first time it appears and read ever after.
CREATE TABLE IF NOT EXISTS offering(
  catalog_path TEXT NOT NULL REFERENCES catalog(path) ON DELETE CASCADE,
  theme_name TEXT NOT NULL REFERENCES theme(name),
  PRIMARY KEY (catalog_path, theme_name)
) WITHOUT ROWID;

-- A row states how much weight stood for a theme at one moment.
-- Choosing a theme contributes the unit case: weight one, counting
-- one, at the moment of the choice. Combining older rows contributes
-- their decayed sum and the number of choices behind it, at the
-- moment of the fold. Both kinds are read the same way.
CREATE TABLE IF NOT EXISTS selection(
  theme_name TEXT NOT NULL REFERENCES theme(name),
  occurred_at INTEGER NOT NULL,
  weight REAL NOT NULL DEFAULT 1.0
    CONSTRAINT selection_weight_is_positive CHECK (weight > 0),
  event_count INTEGER NOT NULL DEFAULT 1
    CONSTRAINT selection_counts_at_least_one CHECK (event_count >= 1),
  PRIMARY KEY (theme_name, occurred_at)
) WITHOUT ROWID;

COMMIT;
