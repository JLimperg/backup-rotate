# backup-rotate

THIS SOFTWARE IS NEITHER STABLE NOR TESTED PROPERLY. Use it only if you've
reviewed the code and believe that it does what you think it does.

## Synopsis

The `backup-rotate` program is a simple script that can be used to keep backups
in specified intervals, such as hourly, daily and weekly. It is intended to be
used together with a utility like [rsnapshot](http://www.rsnapshot.org/) that
creates hourly incremental backups.  Its advantage over the builtin rsnapshot
rotation mechanism is that it handles 'missed' backups (where the computer was
not turned on when the backup would have been made) gracefully.

## Configuration

`backup-rotate` must be configured by editing some constants in the main source
file, `src/BackupRotate.hs`. In particular:

* `backupRoot` is the directory in which all your backups are stored.
* `dateFormat` is the format used for parsing backup dates. See below for where
  this format is used.
* `intervals` is a definition of the time intervals for which backups should
  be kept. Per default this is `hourly`, `daily` and `weekly`.

The following instructions assume that you know some basic Haskell.

### Intervals

For each interval you define, backups from a customisable number of instances
of the interval are kept. For example, if you define a weekly interval with a
`keep` property of `5`, then `backup-rotate` will try to keep the first backups
of the past 5 weeks.

Each interval has the following properties:

* `iName`: The interval's name. This will be used as a prefix when creating
  backup directories. For instance, backups from the `weekly` interval will be
  named `weekly.0`, `weekly.1` and so forth.
* `iKeep`: This property determines how many backups are kept for the interval.
* `iPredicate`: A function that takes two backups and returns `True` if they
  are both within one instance of the interval, `False` otherwise.
  For example, the `iPredicate` for the `weekly` interval should
  return `True` if two backups were created in the same week of the
  same year. The default configuration uses a helper function that
  should be more or less self-explanatory.

Intervals are ordered hierarchically, with the first having the lowest
precedence.  If two intervals claim the same backup for themselves, the one
with higher precedence (i.e. defined after the other) will get it. For
instance, in the default configuration a directory will only be renamed to
`hourly.#` if both the `weekly` and `daily` intervals already have enough
backups to fulfill their `keep` limit.

## Usage

`backup-rotate` assumes the following directory structure:

    backupRoot
    |-- daily.0
    |   |-- date
    |   |-- dir1
    |   `-- dir2
    |-- hourly.0
    |   |-- date
    |   |-- dir1
    |   `-- dir2
    |-- hourly.1
    |   |-- date
    |   |-- dir1
    |   `-- dir2
    |-- weekly.0
    |   |-- date
    |   |-- dir1
    `   `-- dir2

Every backup directory (i.e. every directory that matches an interval name with
a positive integer appended) must contain a plain text file named `date` whose
sole content is the date and time on which the backup was created. The date
must be given in the format determined by `dateFormat`, per default this is
`%Y-%m-%d %H:%M` (i.e. `2013-02-01 22:00`). Note that your backup program
probably does not create such a file automatically.

Given this directory structure, `backup-rotate` will gather all the backup
directories and rename them according to the intervals you've defined (see
'Configuration' above).

When the program is finished renaming, all backups that have not been renamed
(i.e. should not be kept) are permanently deleted.

## Installation

The following steps assume that you've got the Haskell Platform installed.
The script has been created with version 2012.4.0.0 but should run on older
versions as well. No GHC-specific extensions have been used.

1. Clone the Github repo:

        $ git clone git://github.com/JLimperg/backup-rotate.git

2. Customise the constants in `src/BackupRotate.hs` to fit your system.
3. Compile and install the program. The following command will install
   the `backup-rotate` binary under `$HOME/bin/`.

        $ cd backup-rotate
        $ cabal install --prefix=$HOME --user

4. Run `backup-rotate` every time you've finished an hourly backup.

## Known Issues and Shortcomings

1. The script does no error handling whatsoever, which means that it will fail
   immediately if anything I/O-related goes wrong. On the upside, this also
   means that if your backups are messed up, it probably won't mess them up any
   further.
2. The script is currently not unit tested.
3. The script can only be customised by editing constants in the source code
   and recompiling. A proper CLI would be more practical.

## License

This program is made available under the MIT License, a copy of which can be
found in the LICENSE file.
