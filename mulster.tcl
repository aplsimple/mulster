#! /usr/bin/env tclsh
#
# This little thing provides multi-line replacements in text files.
#
# See README for details.
#
# License: MIT.
#
#############################################################################

namespace eval mulster {

  variable _ruff_preamble {
 This little thing provides multi-line replacements in text files.

 It's called this way:

     tclsh mulster.tcl [options] fileini

 where:

   `fileini` is a name of file containing options for replacements, e.g.

     INFILE=input file name.txt
     OUTFILE=output file name.txt
     IN=BEGIN(1,1)
       line #1 to find
       line #2 to find
     IN=END
     OUT=BEGIN
       line #1
     OUT=END

   `options` are:

     -infile input-file-name
     -outfile output-file-name
     -mode exact | exact0 | glob | regexp
     -backup 0 | <dir>
     -keep 0 | 1 | false | true
     -single 0 | 1 | false | true
     -charset charsetName (e.g. cp1251)
     -lineend lineEnding (e.g. \r\n)
     --

 See README for details.

 The `mulster::Mulster` class provides two methods to be called from Tcl module.

 1. The `main` method performs the multi-line replacements. Declared as:

     method main {fileini {mode 1} {backup BAK} {keep 0} {single 0} {charset ""} {lineend ""}}

  where:

  `fileini` is a file name of options

  `mode` = 0 (exact0, EXACT0) to match exact, without leading/tailing spaces

  `mode` = 1 (exact, EXACT) to match exact, with all their leading/tailing spaces

  `mode` = 2 (glob, GLOB) to match glob pattern

  `mode` = 3 (regexp, re, REGEXP, RE) to match regexp pattern

  `mode` = regexp-- to match regexp pattern and call `regsub`

  `mode` = regexp-all to match regexp pattern and call `regsub -all`

  `mode` = regexp-nocase to match regexp pattern and call `regsub -nocase`

  `mode` = regexp-expanded to match regexp pattern and call `regsub -expanded`

  regexp `mode` can be combined, e.g. "regexp-all-nocase"

  `backup 0` means no input files' backuping

  `backup dir` means backuping to *dir* directory

  `keep`  if 1, keeps input files' attributes/times in output files

  `single` if 1, sets a standard string-for-string replacements

  `charset` is a charset of input files, e.g. cp1251

  `lineend` sets characters to end lines, e.g. \r\n (by default \n)

 2. The `mulsterList` method performs in-memory replacements in a list. Declared as:

     method mulsterList {lcont lin lout {r1 0} {r2 0} {mode 1} {single 0}}

  where:

 `lcont` is a list to be processed

 `lin` is a list of input lines (replaced)

 `lout` is a list of output lines (replacing)

  `r1` and `r2` set a range of replacements

  `mode` and `single` are described in `main` method above.

 The `mulsterList` method returns a list processed.
 }

 proc synopsis {} {

  # Puts a synopsis of mulster utility.

  variable _ruff_preamble
  puts $_ruff_preamble

 }

}

oo::class create mulster::Mulster {

  variable _MMM

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method init {infile outfile args} {

    # Initializes processing files.
    #
    #   infile - input file name
    #   outfile - output file name
    #   args - optional arguments

    array set _MMM {}
    my InitIO
    if {$infile ne ""} {
      set _MMM(fin) 1
      set _MMM(infile) $infile
    }
    if {$outfile ne ""} {
      set _MMM(fout) 1
      set _MMM(outfile) $outfile
    }
    set _MMM(IN_FILE)   INFILE=   ;# pattern for in file name
    set _MMM(OUT_FILE)  OUTFILE=  ;# pattern for out file name
    set _MMM(IN_BEGIN)  IN=BEGIN  ;# pattern for in lines start
    set _MMM(IN_END)    IN=END    ;# pattern for in lines finish
    set _MMM(OUT_BEGIN) OUT=BEGIN ;# pattern for out lines start
    set _MMM(OUT_END)   OUT=END   ;# pattern for out lines finish
    set _MMM(MODE)      MODE=     ;# pattern for exact/glob/regexp mode
    set _MMM(BACKUP)    BACKUP=   ;# pattern for backup dir name
    set _MMM(KEEP)      KEEP=     ;# pattern for keep mode
    set _MMM(SINGLE)    SINGLE=   ;# pattern for single mode
    set _MMM(CHARSET)   CHARSET=  ;# pattern for charset mode
    set _MMM(LINEEND)   LINEEND=  ;# pattern for lineend mode
    set _MMM(GRP1) {(.+)}           ;# RE group for a file name
    set _MMM(GRP2) {\((.+),(.+)\)}  ;# RE groups for range
    set _MMM(DEBUG) false         ;# DEBUG mode
    if { [self next] != {} } {
      return [next {*}$args]
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method getSearchMode {strmode} {

    # Gets a search mode as numeric from string.
    #
    #   strmode - search mode in a string form ("exact", "RE" etc.)
    #
    # Returns the numeric mode.
    #
    # Numeric mode:
    #   0 - to match exact, without their leading/tailing spaces (str trimmed)
    #   1 - to match exact, with all their leading/tailing spaces
    #   2 - to match glob
    #   3 - to match regexp for IN= lines and substitute by OUT= lines
    #   4 - to match regexp -all and call 'regsub -all'
    #   8 - to match regexp -nocase and call 'regsub -nocase'
    #  16 - to match regexp -expanded and call 'regsub -expanded'
    #
    # The mode can combine 4,8,16, e.g. 12 means "regexp -all -nocase".

    if {[string is integer $strmode]} {return $strmode}
    set mode 0
    switch -regexp -nocase -matchvar am $strmode {
      ^1$ - ^exact$   {set mode 1}
      ^2$ - ^glob$    {set mode 2}
      ^3$ - ^regexp$ - ^re$ {set mode 3}
      ^regexp\\s*(-.*)$ - ^re\\s*(-.*) {
        set am [split [string toupper [lindex $am 1]] " -"]
        if {"A" in $am || "ALL" in $am} { incr mode 4 }
        if {"N" in $am || "NOCASE" in $am} { incr mode 8 }
        if {"E" in $am || "EXPANDED" in $am} { incr mode 16 }
        incr mode 32  ;# to call 'regsub' anyway
      }
    }
    return $mode
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method ReOptions {mode} {

    # Gets regexp options for a mode.
    #
    #   mode - a search mode (as defined in getSearchMode method)
    #
    # Returns regexp options according to `mode`.
    #
    # See also:
    #   getSearchMode

    set reo ""
    if {$mode & 4}  { append reo " -all" }
    if {$mode & 8}  { append reo " -nocase" }
    if {$mode & 16} { append reo " -expanded" }
    return $reo
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method CompareStr {mode str pattern} {

    # Compares `str` and `pattern` according to mode.
    #
    #   mode - a search mode (as defined in getSearchMode method)
    #   str - a string to compare with `pattern`
    #   pattern - a pattern
    #
    # Returns true if the `str` is matching `pattern`, otherwise - false.
    #
    # See also:
    #   getSearchMode

    if {$mode==0 && [string trim $str] eq $pattern} {
      return true
    } elseif {$mode==1 && $str eq $pattern} {
      return true
    } elseif {$mode==2 && [string match $pattern $str]} {
      return true
    } elseif {$mode>2} {
      return [regexp {*}[my ReOptions $mode] $pattern $str]
    }
    return false
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method InitIO {} {

    # Initializes input & output files processing.

    set _MMM(fin) 0            ;# flag of "input file defined"
    set _MMM(fout) 0           ;# flag of "output file defined"
    set _MMM(inlist) [list]    ;# in lines list
    set _MMM(outlist) [list]   ;# out lines list
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method getBoolean {val {defval true}} {

    # Checks and gets a boolean value.
    #
    #   val - a value to be checked
    #   defval - a default value
    #
    # Returns a normalized value.

    if {$val ni {0 1 false true}} {set val $defval}
    return $val
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method main {fileini {mode 1} {backup BAK} {keep 0} \
  {single 0} {charset ""} {lineend ""}} {

    # Processes files according to the options of 'fileini' file.
    #
    #   fileini - options file name
    #   mode - a search mode (as defined in getSearchMode method)
    #   backup - if 0, means no backuping, otherwise - backup directory
    #   keep - if 1, keeps input files' attributes/times in output files
    #   single - if 1, standard 'one string for one string' replacements
    #   charset - a charset of input files (e.g. cp1251)
    #   lineend - a line ending of input files (e.g. \r\n)
    #
    # See also:
    #   getSearchMode
    #   OptionIs

    set mode [my getSearchMode $mode]
    set keep [my getBoolean $keep]
    set single [my getBoolean $single]
    if {!($_MMM(fin) + $_MMM(fout))} { my InitIO }
    set state NONE
    set _MMM(st) [set comments ""]
    set _MMM(nl) 0
    set _MMM(fileini) $fileini
    set chini [open $fileini]
    foreach _MMM(st) [split [read $chini] \n] {
      incr _MMM(nl)
      switch $state {
        NONE {
          # check for finish
          if {[regexp -nocase "^\\s*MODE=EXIT" $_MMM(st)]} {
            break
          # check for debug mode
          } elseif {[regexp -nocase "^\\s*MODE=DEBUG" $_MMM(st)]} {
            set _MMM(DEBUG) true
          # check for comments
          } elseif {[regexp "^\\s*#" $_MMM(st)]} {
            set comments "\n$_MMM(st)"
          } elseif {[my OptionIs $_MMM(MODE) $_MMM(GRP1)]} {
          # check for mode
            set mode [my getSearchMode $_MMM(match1)]
          # check for backup dir
          } elseif {[my OptionIs $_MMM(BACKUP) $_MMM(GRP1)]} {
            my FlushOut $mode $keep $charset $lineend $backup
            set backup $_MMM(match1)
          # check for keep state
          } elseif {[my OptionIs $_MMM(KEEP) $_MMM(GRP1)]} {
            my FlushOut $mode $keep $charset $lineend $backup
            set keep [my getBoolean $_MMM(match1)]
          # check for charset
          } elseif {[my OptionIs $_MMM(CHARSET) $_MMM(GRP1)]} {
            my FlushOut $mode $keep $charset $lineend $backup
            set charset $_MMM(match1)
          # check for lineend
          } elseif {[my OptionIs $_MMM(LINEEND) $_MMM(GRP1)]} {
            my FlushOut $mode $keep $charset $lineend $backup
            set lineend $_MMM(match1)
          # check for single state
          } elseif {[my OptionIs $_MMM(SINGLE) $_MMM(GRP1)]} {
            set single [my getBoolean $_MMM(match1)]
          # check for input file
          } elseif {[my OptionIs $_MMM(IN_FILE) $_MMM(GRP1)]} {
            my FlushOut $mode $keep $charset $lineend $backup
            set _MMM(fin) 1
            set _MMM(infile) $_MMM(match1)
          # check for output file
          } elseif {[my OptionIs $_MMM(OUT_FILE) $_MMM(GRP1)]} {
            my CheckFiles 1
            set _MMM(fout) 1
            set _MMM(outfile) $_MMM(match1)
          # check for input lines beginning
          } elseif {[my OptionIs $_MMM(IN_BEGIN)] || \
                    [my OptionIs $_MMM(IN_BEGIN) $_MMM(GRP2)]} {
            my CheckFiles 1
            my CheckFiles 2
            lappend _MMM(inlist) [list $_MMM(match1) $_MMM(match2) \
              $single $charset $lineend $mode $comments]
            set comments ""
            set state IN
          # check for output lines beginning
          } elseif {[my OptionIs $_MMM(OUT_BEGIN)]} {
            my CheckFiles 1
            my CheckFiles 2
            lappend _MMM(outlist) [list]
            set state OUT
          }
        }
        IN {
          # check for input lines ending
          if {[my OptionIs $_MMM(IN_END)]} {
            set state NONE
          } else {
            # collect the input lines
            set curl [lindex $_MMM(inlist) end]
            lappend curl $_MMM(st)
            set _MMM(inlist) [lreplace $_MMM(inlist) end end $curl]
          }
        }
        OUT {
          # check for output lines ending
          if {[my OptionIs $_MMM(OUT_END)]} {
            set state NONE
          } else {
            # collect the output lines
            set curl [lindex $_MMM(outlist) end]
            lappend curl $_MMM(st)
            set _MMM(outlist) [lreplace $_MMM(outlist) end end $curl]
          }
        }
      }
    }
    # flush out the collected lines if any
    my FlushOut $mode $keep $charset $lineend $backup
    close $chini
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method mulsterList {lcont lin lout {r1 0} {r2 0} {mode 1} {single 0}} {

    # Performs replacements in a list.
    #
    #   lcont - a list to be processed
    #   lin - input lines' list
    #   lout - output lines' list
    #   r1 - first index of replacements to do
    #   r2 - last index of replacements to do
    #   mode - a search mode (as defined in getSearchMode method)
    #   single - if true, means a standard 'string for string' replacements
    #
    # Returns a list with replacements made.

    set _MMM(repls) 0
    set leni [llength $lin]
    if {!$leni} {
      return $lcont  ;# nothing to replace
    }
    set lres [list]  ;# resulting list
    set sti0 [lindex $lin 0]
    if {$mode==0} {set sti0 [string trim $sti0]} ;# for a quick 1st check
    set leno [llength $lout]
    set lenc [llength $lcont]
    set ic [set ifnd 0]
    while {$ic < $lenc} {
      set stc [lindex $lcont $ic]
      if { [my CompareStr $mode $stc $sti0] } { ;# 1st line found
        for {set ii [set found 1]} {$ii<$leni && $found} {incr ii} {
          if {[set ic2 [expr {$ic + $ii}]] >= $lenc} {
            set found 0  ;# out of lcont's range
          } else {
            set sti [lindex $lin $ii]
            set sto [lindex $lcont $ic2]
            if {$mode==0} {  ;# exact0 is 'trimmed exact' mode
              set found [my CompareStr $mode $sto [string trim $sti]]
            } else {
              set found [my CompareStr $mode $sto $sti]
            }
          }
        }
        if {$found} {
          incr ifnd
          ;# check a found ifnd-th bunch for the range (r1,r2)
          if {$ifnd>=$r1 && ($ifnd<=$r2 || !$r2)} {
            set rinc 1
            for {set io 0} {$io<$leno} {incr io} {
              set stout [lindex $lout $io]
              if {$mode>3} {
                set rinc [regsub {*}[my ReOptions $mode] \
                  [lindex $lin $io] $stc $stout stout]
              }
              lappend lres $stout
            }
            incr ic $leni  ;# go to a line just following the 'lin' bunch
            incr _MMM(repls) $rinc
          } else {         ;# not in the range r1-r2, so skip this IN=/OUT=
            for {set ii 0} {$ii<$leni} {incr ii} {
              lappend lres [lindex $lcont $ic]
              incr ic
            }
          }
          continue
        }
      }
      if {$single} {   ;# do standard string replacement
        foreach si $lin so $lout {
          set stc2 [string map [list $si $so] $stc]
          if {$stc2 ne $stc} {
            incr ifnd
            ;# check a found ifnd-th bunch for the range (r1,r2)
            if {$ifnd>=$r1 && ($ifnd<=$r2 || !$r2)} {
              set stc $stc2
              incr _MMM(repls)
            }
          }
        }
      }
      lappend lres $stc  ;# this line is not the 1st of 'lin' bunch
      incr ic
    }
    return $lres
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method OptionIs {pattern {mvalue ""}} {

    # Checks if a string of option matches a pattern.
    #
    #   pattern - for matching modes ("INFILE=", "IN=BEGIN" etc.)
    #   mvalue - for matching mode values ("inp_file.txt", "(1,1)" etc.)
    #
    # The _MMM(st) is an input string to be checked on `pattern`.
    # The _MMM(GRP1) is a input string to be checked on `mvalue`.
    # The _MMM(match1) and _MMM(match2) are output values of range (r1-r2).
    #
    # Returns true, if a string matches a pattern, otherwise - false.
    #
    # See also:
    #   main

    if {![regexp "^${pattern}*" $_MMM(st)]} {
      return 0
    }
    set _MMM(match1) 0
    set _MMM(match2) 0
    if {$mvalue==""} {
      return [regexp "^${pattern}\$" $_MMM(st)]
    }
    if {[regexp "^${pattern}\$" $_MMM(st)] && $mvalue==""} {
      return 1
    }
    if {[regexp "^${pattern}$mvalue\$" $_MMM(st) tot r1 r2]} {
      if {$mvalue==$_MMM(GRP1)} {
        return [string length [set _MMM(match1) $r1]]
      }
      if {[string is digit $r1] && [string is digit $r2]} {
        set _MMM(match1) $r1
        set _MMM(match2) $r2
        return 1
      }
    }
    return 0
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method FlushOut {mode keep charset lineend backup} {

    # Flushes the current file(s) and initializes the mulster's variables.
    #
    #   mode - a search mode (as defined in getSearchMode method)
    #   keep - a keep mode
    #   charset - a charset of processed file
    #   lineend - a line ending of processed file
    #   backup - a backup mode
    #
    # Uses _MMM(fin) and $_MMM(fout) as flags 'need of flush'.
    # If they are set, calls `RecurseProc` to flush the output file(s).
    # 
    # See also:
    #   main
    #   InitIO
    #   RecurseProc

    if {$_MMM(fin) || $_MMM(fout)} {
      my CheckFiles 1
      my CheckFiles 2
      set root [file dirname $_MMM(infile)]
      set patt [file tail $_MMM(infile)]
      my RecurseProc $root $root $patt $_MMM(outfile) \
        $mode $keep $charset $lineend $backup
    }
    my InitIO
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method FileAttributes {fname {attrs ""} {atime ""} {mtime ""} } {

    # Gets/sets attributes of a file.
    #
    #   fname - a name of file
    #   attrs - file attributes
    #   atime - atime attribute of file
    #   mtime - mtime attribute of file
    #
    # Returns a list of attributes of the files if only `fname` is set.
    #
    # With `attrs`, `atime`, `mtime` set, the method sets the attributes.

    if {$attrs==""} {
      set attrs [file attributes $fname]
      return [list $attrs [file atime $fname] [file mtime $fname]]
    }
   file attributes $fname {*}$attrs
   file atime $fname $atime
   file mtime $fname $mtime
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method ProcFile {infile outfile mode keep charset lineend backup} {

    # Mulsters an input file to make an output file.
    #
    #   infile - a name of input file
    #   outfile - a name of output file
    #   mode - search mode of mulster
    #   keep - keep option of mulster
    #   charset - charset of file
    #   lineend - line ending characters
    #   backup - backup option of mulster
    #
    # See also:
    #   FlushOut

    puts "\nMULSTER: $infile ==> $outfile"
    if {$keep} {
      lassign [my FileAttributes $infile] attrs atime mtime
    }
    set le [string map [list \\n \n \\r \r] $lineend]
    my BackupFile $backup $infile $charset $le
    set chan [open $infile]
    if {$charset ne ""} {
      chan configure $chan -encoding $charset
    }
    set lcont [split [read $chan] \n]
    close $chan
    for {set il 0} {$il<[llength $_MMM(inlist)]} {incr il} {
      set lin [lindex $_MMM(inlist) $il]
      set lout [lindex $_MMM(outlist) $il]
      # r1, r2, ... are options per a IN=/OUT= block
      lassign $lin r1 r2 single charset lineend mode comments
      if {$comments ne ""} { puts $comments }
      # other options go after them
      set lin [lrange $lin 7 end]
      set lcont [my mulsterList $lcont $lin $lout $r1 $r2 $mode $single]
      set t [expr {$il+1}]
      puts -nonewline "Replacement #$t: "
      if {[string length $t$_MMM(repls)]>3} {set t "\t"} {set t "\t\t"} 
      if {$mode>2} {set mode "regexp[my ReOptions $mode]"}
      append t "SINGLE=$single, MODE=${mode}"
      if {$_MMM(repls)==0} {
        puts "NOTHING DONE!\t$t"
      } else {
        if {$_MMM(DEBUG)} {
          puts "$_MMM(repls) done$t"
        } else {
          puts "$_MMM(repls) done"
        }
      }
    }
    if {[catch {set chan [open $outfile w]} err]} {
      puts "Couldn't create $outfile\n    $err"
    } else {
      if {$charset ne ""} {
        chan configure $chan -encoding $charset
      }
      foreach stout $lcont {
        if {[incr iamidiotofFMD]>1} {
          if {$le eq ""} {
            puts $chan ""
          } else {
            puts -nonewline $chan $le
          }
        }
        puts -nonewline $chan $stout
      }
      close $chan
      if {$keep} {
        my FileAttributes $outfile $attrs $atime $mtime
      }
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method RecurseProc {root dirname inpatt outpatt mode \
   keep charset lineend backup} {

    # Scans recursively a directory for files as glob patterns.
    #
    #   root - a root directory name
    #   dirname - current directory name
    #   inpatt - glob pattern of input files
    #   outpatt - pattern of output files (only "*" allowed)
    #   mode - search mode of mulster
    #   keep - keep option of mulster
    #   charset - charset of file
    #   lineend - line ending characters
    #   backup - backup option of mulster
    #
    # For each found file, gets an output file name, makes an output
    # directory and calls the mulstering procedure.
    #
    # See also:
    #   FlushOut

    if {[string first * $outpatt]<0} {
      ;# no patterns, simple file-to-file processing
      my ProcFile [file join $root $inpatt] $outpatt \
        $mode $keep $charset $lineend $backup
      return
    }
    foreach dir [glob -nocomplain [file join $dirname *]] {
      if {[file isdirectory $dir]} {
        my RecurseProc $root $dir $inpatt $outpatt \
          $mode $keep $charset $lineend $backup
      }
    }
    # no dirs anymore
    foreach filetempl [split $inpatt ", "] {
      if {![catch { \
      set files [glob -nocomplain [file join $dirname $filetempl]]}]} {
        foreach f $files {
          # compose the output file name from input file's and output pattern
          set outf [file tail $outpatt]
          set inpf [file rootname [file tail $f]]
          set outf [string map [list * $inpf] $outf]
          # subdirectory of input file, relative to the root
          set r1 [file dirname [file normalize $f]]
          set r2 [file normalize $root]
          set sdir [string range $r1 [string len $r2]+1 end]
          # join all parts of output file name:
          # root output directory + input subdirectory + output file name
          set outd [file join [file dirname $outpatt] $sdir]
          catch {file mkdir $outd}
          set outf [file join $outd $outf]
          my ProcFile $f $outf $mode $keep $charset $lineend $backup
        }
      }
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method BackupFile {backup filename charset le} {

    # Backs up the input file.
    #
    #   backup - if 1, means "do backup"
    #   filename - a name of file to backup
    #   charset - a charset of the file
    #   le - a line ending characters (or "")

    if {$backup!="0" && $backup!=""} {
      lassign [my FileAttributes $filename] attrs atime mtime
      set chan [open $filename]
      if {$charset ne ""} {
        chan configure $chan -encoding $charset
      }
      set cont [read $chan]
      close $chan
      catch {file mkdir $backup}
      set bakfile [file join $backup [file tail $filename]]
      set chan [open $bakfile w]
      if {$charset ne ""} {
        chan configure $chan -encoding $charset
      }
      if {$le ne ""} {
        set cont [string map [list \n $le] $cont]
      }
      puts -nonewline $chan $cont
      close $chan
      my FileAttributes $bakfile $attrs $atime $mtime
      puts "BACKUP : $filename ==> $bakfile"
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method CheckFiles {ncheck} {

    # Checks a setting of input / output file.
    #
    #   ncheck - if 1, checks input file; if 2, checks output file
    
    switch $ncheck {
      1 {my CheckVal $_MMM(fin) 1 "Input file not defined"}
      2 {my CheckVal $_MMM(fout) 1 "Output file not defined"}
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method CheckVal {val propval msg} {

    # Checks if `val` is equal to `propval` and puts the error if not.
    #
    #   val - a value checked
    #   propval - a value checking
    #   msg - an error message

    if {$val != $propval} {
      if {[info exists _MMM(nl)]} {
        set stnl "\n File $_MMM(fileini)\n Line #$_MMM(nl) "
      } else {
        set stnl ""
      }
      error "\n\nERROR in [self] object of Mulster class:\n $msg$stnl\n\n"
    }
  }

}

#############################################################################
# main program ...mmm

if {[info exist ::argv0] && $::argv0==[info script]} {
  if {$::argc<1} {
    mulster::synopsis
    exit
  }
  mulster::Mulster create mul
  array set options {-mode 1 -backup BAK -keep 0 -single 0 \
    -charset {} -lineend {} fn {}}
  set infile [set outfile ""]
  set off 0
  foreach {opt val} $::argv {
    if {$off} {
      set options(fn) [string trim "$options(fn) $opt $val"]
      continue
    }
    switch -exact $opt {
      -m - -mode    { set options(-mode) [mul getSearchMode $val] }
      -b - -backup  { set options(-backup) $val }
      -k - -keep    { set options(-keep) [mul getBoolean $val] }
      -s - -single  { set options(-single) [mul getBoolean $val] }
      -c - -charset { set options(-charset) $val }
      -l - -lineend { set options(-lineend) $val }
      -i - -infile  { set infile $val }
      -o - -outfile { set outfile $val }
      -- {
        set off 1
        set options(fn) $val
      }
      default {
        set off 1
        set options(fn) [string trim "$options(fn) $opt $val"]
      }
    }
  }
  if {$outfile eq "" && $infile ne ""} { set outfile $infile }
  mul init $infile $outfile
  mul main $options(fn) $options(-mode) $options(-backup) \
    $options(-keep) $options(-single) $options(-charset) $options(-lineend)
  mul destroy
}

#############################################################################
#
# for "Run me" of e_menu:
#
#-ARGS0:
#-ARGS0: -k 1 -mode glob test/test12ini
#-ARGS1: -b 0 tasks/mulster-tke
#-ARGS2: -b 0 tasks/mulster-geany
#ARGS3: -b 0 tasks/mulster-ruff
#
#############################################################################
