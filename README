# backup-rotate

THIS SOFTWARE IS NEITHER STABLE NOR TESTED PROPERLY. Use it only if you've
reviewed the code and believe that it does what you think it does.

## Synopsis

The `backup-rotate` program is a simple script that can be used to keep backups
in specified intervals, such as hourly, daily and weekly. It is intended to be
used together with a utility like [rsnapshot](http://www.rsnapshot.org/) that
creates hourly incremental backups.  Its advantage over the builtin rsnapshot
rotation mechanism is that it handles 'missed' backups (where the computer was
not turned on at when the backup would have been made) gracefully.

## Configuration

`backup-rotate` must be configured by editing some constants in the main source
file, `backup-rotate.hs`. In particular:

* `backupRoot` is the directory in which all your backups are stored.
* `dateFormat` is the format in which the date is stored. See below for how
  this property is used.
* `intervals` is a definition of the time intervals for which backups should
  be kept. Per default this is `hourly`, `daily` and `weekly`.

### Intervals

For each interval you define, backups from a customisable number of those
intervals are kept. For instance, if you define a weekly interval with a `keep`
property of `5`, then `backup-rotate` will try to keep the first backup in each
week for the past 5 weeks.

Each interval therefore consists of the following properties:

* `iName`: The interval's name. This will be used as the directory prefix
   when creating backup directory. For instance, backups from the `weekly`
   interval will be named `weekly.0`, `weekly.1` and so forth.
* `iKeep`: Backups from how many intervals should `backup-rotate` keep?
* `iPredicate`: A function that takes two backups and returns `True` if they
   are both within one instance of an interval, `False` otherwise.
   For instance, the `iPredicate` for the `weekly` interval should
   return `True` if two backups were made in the same week of the
   same year. The default configuration uses a little helper function that
   should be more or less self-explanatory.

Intervals are ordered hierarchically, with the first being overwritten by all
the others, the second by all but the first and so on. By 'overwritten' I mean
that if two intervals claim the same backup for themselves, the one with the
higher precedence (i.e. defined after the other) will get it. For instance,
in the default configuration a directory will only be renamed to `hourly.n` if
both the `weekly` and `daily` intervals already have enough backups to fulfill
their `keep` limit.

## Usage

`backup-rotate` assumes the following directory structure:

    /path/to/backup-root
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

Every backup directory (i.e. every directory that matches
`(hourly|daily|weekly)\.[0-9]+$`) must contain a plain text file named `date`
whose sole content is the date and time on which the backup was created. The
date must be given in the format determined by `date-format`, per default this
is `%Y-%m-%d %H:%M` (so for example `2013-02-01 22:00`). Note
that `rsnapshot` does not create such a file automatically.

Given this directory structure, `backup-rotate` will gather all the backup
directories and rename them according to when they were created and according
to the intervals you've defined. See the Configuration section above for what
exactly this means.

When the program is finished renaming, all backups that have not been renamed
(i.e. should not be kept) are permanently deleted.

## Installation

The following steps assume that you've got the Haskell Platform installed.
The script has been created with version 2012.4.0.0 but should run on older
versions as well. No GHC-specific extensions have been used.

1. Clone the Github repo:

       $ git clone git://github.com/JLimperg/backup-rotate.git

2. Customise the constants in `backup-rotate.hs` to fit your system.
3. Compile the Haskell sources

       $ cd backup-rotate
       $ ghc --make backup-rotate.hs

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
