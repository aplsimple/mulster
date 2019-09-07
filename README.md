
This little thing provides multi-line replacements in text files.
Also, it provides in-memory replacements, on a list of strings.

Although there are tons of similar utilities, maybe this one has
its own goods.

It would be useful when you need:
  1) replace periodically some bunches of strings in some texts;
  2) modify a snapshot of some software without its cloning/forking;
  3) share your little modifications with your colleagues, without
     resorting to those great GIT, MERCURIAL, FOSSIL etc.
  4) perform in-memory multi-line replacements.

It's called this way:
  tclsh mulster.tcl ?options? fileini
where:
  fileini is a name of file containing settings for replacements.

The fileini has the following structure:

  INFILE=<input file name>
  OUTFILE=<output file name>

  IN=BEGIN
    line #1 to find
    line #2 to find
    ...
    line #N1 to find
  IN=END
  OUT=BEGIN
    line #1 of replacement
    line #2 of replacement
    ...
    line #N2 of replacement
  OUT=END

  IN=BEGIN(r1,r2)
  ....
  IN=END
  OUT=BEGIN
  ...
  OUT=END
  ...

  BACKUP=new backup dir
  EXACT=1
  KEEP=0
  INFILE=<input file2 name>
  OUTFILE=<output file2 name>
  ...

The INFILE= and OUTFILE= set the names of input & output files.

If <input file name> is equal to <output file name> all modifications
are performed on the same file.

All strings between current IN=BEGIN and IN=END are replaced with
strings between the next OUT=BEGIN and OUT=END. The sequence of
INFILE=, OUTFILE=, IN=, OUT= [, IN=, OUT= ...] is set for each
processed file.

The IN=BEGIN(r1,r2) form means that a range of found matches should
be processed as follows:
  IN=BEGIN(r1,r2) - r1-th match through r2-th one
  IN=BEGIN(r1,0)  - r1-th match through the last one
  IN=BEGIN(0,r2)  - the same as IN=BEGIN(1,r2)
  IN=BEGIN(1,1)   - first match only
  IN=BEGIN(0,0)   - all matches; the same as IN=BEGIN

All strings outside of
  INFILE=
  OUTFILE=
  BACKUP=
  EXACT=
  KEEP=
  IN=BEGIN through IN=END
  OUT=BEGIN through OUT=END
are ignored (being sort of comments).

For example, applying the following fileini:
  INFILE=modul1.tcl
  OUTFILE=modul2.tcl
  IN=BEGIN
    proc1 $a $b
    proc2 $a2 $b2
  IN=END
  OUT=BEGIN
    proc3 $a $b $a2 $b2  ;# <=====REPLACED
  OUT=END

... to the modul1.tcl containing:
  1st-comm
  2nd-comm
  proc1 $a $b
  proc2 $a2 $b2
  next-comm
  #... other commands
  proc1 $a $b
  proc2 $a2 $b2

... we get the modul2.tcl containing:
  1st-comm
  2nd-comm
  proc3 $a $b $a2 $b2  ;# <=====REPLACED
  next-comm
  #... other commands
  proc3 $a $b $a2 $b2  ;# <=====REPLACED

The options of
  tclsh mulster.tcl ?options? fileini
are following:
  -exact 1 means that IN-lines should be matched exactly, with
         all their leading/tailing spaces as specified in
         IN=BEGIN blocks; by default the lines are trimmed at
         finding their matches;
  -backup <dir> means that the original input files are
         backed up into the <dir> directory;
  -backup 0 means that the original input files are NOT backed up
         at all (by default they are backed up to BAK directory);
  -keep 1 means that input files' attributes/times will be
         saved in output files; by default they are not kept;
  -- switches options off (for fileini)

The -exact, -backup and -keep options can be redefined in 'fileini',
e.g.
  ...
  BACKUP=BAK/new backup dir
  EXACT=1
  KEEP=1
  INFILE=<input file2 name>
  OUTFILE=<output file2 name>
  ...
so that these options can be individual for any file(s).

Note: if the mulster comes across BACKUP=, EXACT=, KEEP= or INFILE=
option in fileini, it flushes all collected changes to the current
output file and starts a new collection of changes for a new file.

So, the order of options is important:

  1) BACKUP=, EXACT= or KEEP= go first if they are set
  2) INFILE= and OUTFILE= go next
  3) IN=BEGIN and IN=END go next
  4) OUT=BEGIN and OUT=END go next
  5) (3) through (4) can be repeated
  6) (1) through (5) can be repeated

Examples:
  tclsh mulster.tcl mulster1_ini
  tclsh mulster.tcl -exact 1 mulster2_ini
  tclsh mulster.tcl -backup ~/BAK mulster3_ini
  tclsh mulster.tcl -exact 1 -backup 0 -keep 1 mulster4_ini

While using the -backup 0 option, please be careful. This mode is
well suitable when:
  - all your input files are not the same as output files;
  - you've made a backup beforehand;
  - you have a nice VCS and are not worried about any data loss.
Otherwise you would take risks of data loss.

The mulster.tcl prints out a log of replacements made (as well as
made not).

---

I run into an appropriate case of 'mulstering' some time ago when it
occured to me to enhance the context action of Geany IDE. Being not
of Geany team nor of their contributors/fans, I couldn't insist on
this enhancement with Github PR (though had tried, without success:)
At that, forking/cloning Geany from Github to perform my home-made
corrections would be an overkill and a waste of time, having in mind
the future releases of Geany. Each time at releasing new Geany to do
the same manipulations? I'm too lazy, so this scenario isn't for me.

But now, with the mulster at hand, all I need is:
  1) download a current Geany snapshot;
  2) 'mulster' some files of it to get the desirable facility;
  3) 'make istall' Geany;
  4) repeat 1)-2)-3) at releasing new Geany versions, all those
     actions being easily automated with shell commands prepared
     beforehand (as well as 'fileini').

Nearly the same thing has repeated with TKE editor. I have its clone
and work at it, but being only a contributor (the author is Trevor
Williams) I cannot implement some of its features which are good
for me and not acceptable for the author. So, I need these facilities
being implemented 'on fly', after pulling TKE from its repository.
Then that my changes made, the mulstered TKE code should be undoed
before pushing my changes to SourceForge.

These pull/push transactions and accompanying mulsterings are made
with one click in a TKE plugin. I need only supervising a short log.

The mulster.rar archive contains both appropriate 'fileini' files to
make the changes in Geany and TKE, which might be used as examples.

---

A bit about using the mulster's code. It's implemented as oo::class
and has two useful methods declared as:

 1) method mulster {fileini {exact 0} {backup BAK} {keep 0}}

    where:
      'fileini' is a file name of options
      'exact' equals to:
        0 - matches of trimmed lines are allowed
        1 - only exact matches are allowed
      'backup' equals to:
        0     - no input files' backuping
        "dir" - backuping to "dir" directory
      'keep' if 1, keeps input files' attributes/times in output files

    This method performs the above described operations.

 2) method mulster1 {lcont lin lout {r1 0} {r2 0} {exact 1}}

    This method performs in-memory replacements in a list ('lcont')
    according to a list of input lines ('lin') and a list of output
    lines ('lout'), in a range of replacements set with 'r1' and 'r2'.
    If 'exact' is 1, the lines are compared to be strongly equal,
    otherwise their leading/tailing spaces are ignored at comparing.

Home page:
  https://aplsimple.bitbucket.io/en/tcl/mulster
