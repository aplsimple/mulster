#! /usr/bin/env tclsh
#
# This little thing provides multi-line replacements in text files.
# It's called this way:
#   tclsh mulster.tcl ?options? fileini
# where:
#   fileini is a name of file containing options for replacements, e.g.
#       INFILE=input file name.txt
#       OUTFILE=output file name.txt
#       IN=BEGIN(1,1)
#         line #1 to find
#         line #2 to find
#       IN=END
#       OUT=BEGIN
#         line #1
#       OUT=END
#   options are:
#       -infile input-file
#       -outfile output-file
#       -mode exact | exact0 | glob | regexp
#       -backup 0 | <dir>
#       -keep 0 | 1 | false | true
#       -single 0 | 1 | false | true
#       -charset charsetName
#       -lineend lineEnding
#       --
# See README for details.
#############################################################################

oo::class create Mulster {

  variable _mulster

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method init {infile outfile args} {

    # Initializes processing files.

    array set _mulster {}
    my InitIO
    if {$infile ne ""} {
      set _mulster(fin) 1
      set _mulster(infile) $infile
    }
    if {$outfile ne ""} {
      set _mulster(fout) 1
      set _mulster(outfile) $outfile
    }
    set _mulster(IN_FILE)   INFILE=   ;# pattern for in file name
    set _mulster(OUT_FILE)  OUTFILE=  ;# pattern for out file name
    set _mulster(IN_BEGIN)  IN=BEGIN  ;# pattern for in lines start
    set _mulster(IN_END)    IN=END    ;# pattern for in lines finish
    set _mulster(OUT_BEGIN) OUT=BEGIN ;# pattern for out lines start
    set _mulster(OUT_END)   OUT=END   ;# pattern for out lines finish
    set _mulster(MODE)      MODE=     ;# pattern for exact/glob/regexp mode
    set _mulster(BACKUP)    BACKUP=   ;# pattern for backup dir name
    set _mulster(KEEP)      KEEP=     ;# pattern for keep mode
    set _mulster(SINGLE)    SINGLE=   ;# pattern for single mode
    set _mulster(CHARSET)   CHARSET=  ;# pattern for charset mode
    set _mulster(LINEEND)   LINEEND=  ;# pattern for lineend mode
    set _mulster(GRP1) {(.+)}           ;# RE group for a file name
    set _mulster(GRP2) {\((.+),(.+)\)}  ;# RE groups for range
    if { [self next] != {} } {
      return [next {*}$args]
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method getSearchMode {strmode} {

    # Gets a search mode as numeric from string.
    # Default is 1 ("exact").

    switch $strmode {
      0 - exact0 - EXACT0           {return 0}
      2 - glob   - GLOB             {return 2}
      3 - regexp - re - REGEXP - RE {return 3}
      default                       {return 1}
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compares `str` and `pattern` according to mode
  #   'mode' :
  #      0 - to match exact, without their leading/tailing spaces (str trimmed)
  #      1 - to match exact, with all their leading/tailing spaces
  #      2 - to match glob
  #      3 - to match regexp

  method CompareStr {mode str pattern} {

    if {$mode==0 && [string trim $str] eq $pattern || \
        $mode==1 && $str eq $pattern || \
        $mode==2 && [lsearch -glob [list $str] $pattern]==0 || \
        $mode==3 && [lsearch -regexp [list $str] $pattern]==0 } {
      return true
    }
    return false
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method InitIO {} {

    # Initializes input & output files processing

    set _mulster(fin) 0            ;# flag of "input file defined"
    set _mulster(fout) 0           ;# flag of "output file defined"
    set _mulster(inlist) [list]    ;# in lines list
    set _mulster(outlist) [list]   ;# out lines list
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  method getBoolean {val {defval true}} {

    # Checks and gets a boolean value

    if {$val ni {0 1 false true}} {set val $defval}
    return $val
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Processes files according to the options of 'fileini' file.
  # Input parameters:
  #   'fileini' is options file name
  #   'mode' :
  #      0 - to match exact, without their leading/tailing spaces
  #      1 - to match exact, with all their leading/tailing spaces
  #      2 - to match glob
  #      3 - to match regexp
  #   'backup' if 0, means no backuping, otherwise - backup directory
  #   'keep' if 1, keeps input files' attributes/times in output files
  #   'single' if 1, standard 'one string for one string' replacements
  #   'charset' sets a charset of input files (e.g. cp1251)
  #   'lineend' sets a line ending of input files (e.g. \r\n)

  method mulster {fileini {mode 1} {backup BAK} {keep 0} \
  {single 0} {charset ""} {lineend ""}} {
    set mode [my getSearchMode $mode]
    set keep [my getBoolean $keep]
    set single [my getBoolean $single]
    if {!($_mulster(fin) + $_mulster(fout))} { my InitIO }
    set state NONE
    set _mulster(st) ""
    set _mulster(nl) 0
    set _mulster(fileini) $fileini
    set chini [open $fileini]
    foreach _mulster(st) [split [read $chini] \n] {
      incr _mulster(nl)
      switch $state {
        NONE {
          # check for mode
          if {[my OptionIs $_mulster(MODE) $_mulster(GRP1)]} {
            set mode [my getSearchMode $_mulster(match1)]
          # check for backup dir
          } elseif {[my OptionIs $_mulster(BACKUP) $_mulster(GRP1)]} {
            my FlushOut $mode $keep $charset $lineend $backup
            set backup $_mulster(match1)
          # check for keep state
          } elseif {[my OptionIs $_mulster(KEEP) $_mulster(GRP1)]} {
            my FlushOut $mode $keep $charset $lineend $backup
            set keep [my getBoolean $_mulster(match1)]
          # check for charset
          } elseif {[my OptionIs $_mulster(CHARSET) $_mulster(GRP1)]} {
            my FlushOut $mode $keep $charset $lineend $backup
            set charset $_mulster(match1)
          # check for lineend
          } elseif {[my OptionIs $_mulster(LINEEND) $_mulster(GRP1)]} {
            my FlushOut $mode $keep $charset $lineend $backup
            set lineend $_mulster(match1)
          # check for single state
          } elseif {[my OptionIs $_mulster(SINGLE) $_mulster(GRP1)]} {
            set single [my getBoolean $_mulster(match1)]
          # check for input file
          } elseif {[my OptionIs $_mulster(IN_FILE) $_mulster(GRP1)]} {
            my FlushOut $mode $keep $charset $lineend $backup
            set _mulster(fin) 1
            set _mulster(infile) $_mulster(match1)
          # check for output file
          } elseif {[my OptionIs $_mulster(OUT_FILE) $_mulster(GRP1)]} {
            my CheckForError 1
            set _mulster(fout) 1
            set _mulster(outfile) $_mulster(match1)
          # check for input lines beginning
          } elseif {[my OptionIs $_mulster(IN_BEGIN)] || \
                    [my OptionIs $_mulster(IN_BEGIN) $_mulster(GRP2)]} {
            my CheckForError 1
            my CheckForError 2
            lappend _mulster(inlist) [list $_mulster(match1) $_mulster(match2) \
              $single $charset $lineend $mode]
            set state IN
          # check for output lines beginning
          } elseif {[my OptionIs $_mulster(OUT_BEGIN)]} {
            my CheckForError 1
            my CheckForError 2
            lappend _mulster(outlist) [list]
            set state OUT
          }
        }
        IN {
          # check for input lines ending
          if {[my OptionIs $_mulster(IN_END)]} {
            set state NONE
          } else {
            # collect the input lines
            set curl [lindex $_mulster(inlist) end]
            lappend curl $_mulster(st)
            set _mulster(inlist) [lreplace $_mulster(inlist) end end $curl]
          }
        }
        OUT {
          # check for output lines ending
          if {[my OptionIs $_mulster(OUT_END)]} {
            set state NONE
          } else {
            # collect the output lines
            set curl [lindex $_mulster(outlist) end]
            lappend curl $_mulster(st)
            set _mulster(outlist) [lreplace $_mulster(outlist) end end $curl]
          }
        }
      }
    }
    # flush out the collected lines if any
    my FlushOut $mode $keep $charset $lineend $backup
    close $chini
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Performs replacements in a list ('lcont') according to a list of
  # input lines ('lin') and  a list of output lines ('lout'), in
  # a range of replacements set with 'r1' and 'r2' parameters.
  #
  #   'mode' :
  #      0 - to match exact, without their leading/tailing spaces
  #      1 - to match exact, with all their leading/tailing spaces
  #      2 - to match glob
  #      3 - to match regexp
  #
  # If 'single' is true, the input lines ('lin') are replaced with output
  # lines ('lout') in a list ('lcont') as standard string replacements.

  method mulster1 {lcont lin lout {r1 0} {r2 0} {mode 1} {single 0}} {
    set _mulster(repls) 0
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
            for {set io 0} {$io<$leno} {incr io} {
              set stout [lindex $lout $io]
              if {$mode==3} {
                regsub -all [lindex $lin $io] $stc $stout stout
              }
              lappend lres $stout
            }
            incr ic $leni  ;# go to a line just following the 'lin' bunch
            incr _mulster(repls)
          } else {
            ;# still not now, so pass this bunch
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
              incr _mulster(repls)
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
  # Checks if a string matches a pattern.
  # If 'group' parameter is set, it is added to 'pattern'.
  # The 'pattern' is for matching "INFILE=", "IN=BEGIN" etc.
  # The 'group' is for matching "inp_file.txt", "(1,1)" etc.

  method OptionIs {pattern {group ""}} {
    if {![regexp "^${pattern}*" $_mulster(st)]} {
      return 0
    }
    set _mulster(match1) 0
    set _mulster(match2) 0
    if {$group==""} {
      return [regexp "^${pattern}\$" $_mulster(st)]
    }
    if {[regexp "^${pattern}\$" $_mulster(st)] && $group==""} {
      return 1
    }
    if {[regexp "^${pattern}$group\$" $_mulster(st) tot r1 r2]} {
      if {$group==$_mulster(GRP1)} {
        return [string length [set _mulster(match1) $r1]]
      }
      if {[string is digit $r1] && [string is digit $r2]} {
        set _mulster(match1) $r1
        set _mulster(match2) $r2
        return 1
      }
    }
    return 0
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flushes the current file and initializes the mulster's variables

  method FlushOut {mode keep charset lineend backup} {
    if {$_mulster(fin) || $_mulster(fout)} {
      my CheckForError 1
      my CheckForError 2
      set root [file dirname $_mulster(infile)]
      set patt [file tail $_mulster(infile)]
      my recurseProc $root $root $patt $_mulster(outfile) \
        $mode $keep $charset $lineend $backup
    }
    my InitIO
  }

  method recurseProc {root dirname inpatt outpatt mode \
   keep charset lineend backup} {
    # Scans reversively a directory for files as glob patterns.
    #   root - a root directory name
    #   dirname - current directory name
    #   inpatt - glob pattern of input files
    #   outpatt - glob pattern of output files
    #   mode, keep, charset, lineend, backup - arguments of mulster
    #
    # For each found file, gets an output file name, makes an output
    # directory and calls the mulstering procedure.

    if {[string first * $outpatt]<0} {
      ;# no patterns, simple file-to-file processing
      my ProcFile [file join $root $inpatt] $outpatt \
        $mode $keep $charset $lineend $backup
      return
    }
    foreach dir [glob -nocomplain [file join $dirname *]] {
      if {[file isdirectory $dir]} {
        my recurseProc $root $dir $inpatt $outpatt \
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

  method ProcFile {infile outfile mode keep charset lineend backup} {
    # Mulsters an input file to make an output file.
    #   infile -
    #   outfile -
    #   mode, keep, charset, lineend, backup - arguments of mulster

    puts "\nMULSTER: $infile ==> $outfile"
    if {$keep} {
      lassign [my FileAttributes $infile] attrs atime mtime
    }
    set le [string map [list \\n \n \\r \r] $lineend]
    my BackupFile $backup $infile $charset $le
    set ch [open $infile]
    if {$charset ne ""} {
      chan configure $ch -encoding $charset
    }
    set lcont [split [read $ch] \n]
    close $ch
    for {set il 0} {$il<[llength $_mulster(inlist)]} {incr il} {
      set lin [lindex $_mulster(inlist) $il]
      set lout [lindex $_mulster(outlist) $il]
      # r1, r2, ... are options per a IN=/OUT= block
      lassign $lin r1 r2 single charset lineend mode
      # other options go after them
      set others [lrange $lin 6 end]
      set lcont [my mulster1 $lcont $others $lout $r1 $r2 $mode $single]
      puts -nonewline "Replacement #[expr {$il+1}]: "
      if {$_mulster(repls)==0} {
        puts "NOTHING CHANGED!"
      } else {
        puts "$_mulster(repls) done"
      }
    }
    if {[catch {set ch [open $outfile w]} err]} {
      puts "Couldn't create $outfile\n    $err"
    } else {
      if {$charset ne ""} {
        chan configure $ch -encoding $charset
      }
      foreach stout $lcont {
        if {[incr iamidiotofFMD]>1} {
          if {$le eq ""} {
            puts $ch ""
          } else {
            puts -nonewline $ch $le
          }
        }
        puts -nonewline $ch $stout
      }
      close $ch
      if {$keep} {
        my FileAttributes $outfile $attrs $atime $mtime
      }
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Gets/sets file attributes

  method FileAttributes {fname {attrs ""} {atime ""} {mtime ""} } {
    if {$attrs==""} {
      set attrs [file attributes $fname]
      return [list $attrs [file atime $fname] [file mtime $fname]]
    }
   file attributes $fname {*}$attrs
   file atime $fname $atime
   file mtime $fname $mtime
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Backs up the input file

  method BackupFile {backup filename charset le} {
    if {$backup!="0" && $backup!=""} {
      lassign [my FileAttributes $filename] attrs atime mtime
      set ch [open $filename]
      if {$charset ne ""} {
        chan configure $ch -encoding $charset
      }
      set cont [read $ch]
      close $ch
      catch {file mkdir $backup}
      set bakfile [file join $backup [file tail $filename]]
      set ch [open $bakfile w]
      if {$charset ne ""} {
        chan configure $ch -encoding $charset
      }
      if {$le ne ""} {
        set cont [string map [list \n $le] $cont]
      }
      puts -nonewline $ch $cont
      close $ch
      my FileAttributes $bakfile $attrs $atime $mtime
      puts "BACKUP : $filename ==> $bakfile"
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Checks if 'val' is equal to 'propval' and puts the error if not

  method CheckForError {ncheck} {
    switch $ncheck {
      1 {my Check1 $_mulster(fin) 1 "Input file not defined"}
      2 {my Check1 $_mulster(fout) 1 "Output file not defined"}
    }
  }

  method Check1 {val propval msg} {
    if {$val != $propval} {
      if {[info exists _mulster(nl)]} {
        set stnl "\n File $_mulster(fileini)\n Line #$_mulster(nl) "
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
    puts "
 This little thing provides multi-line replacements in text files.

 It's called this way:
   tclsh mulster.tcl ?options? fileini
 where:
   fileini is a name of file containing options for replacements, e.g.
       INFILE=input file name.txt
       OUTFILE=output file name.txt
       IN=BEGIN(1,1)
         line #1 to find
         line #2 to find
       IN=END
       OUT=BEGIN
         line #1
       OUT=END
   options are:
       -infile input-file-name
       -outfile output-file-name
       -mode exact | exact0 | glob | regexp
       -backup 0 | <dir>
       -keep 0 | 1 | false | true
       -single 0 | 1 | false | true
       -charset charsetName
       -lineend lineEnding
       --

 See README for details.
"
    exit
  }
  Mulster create mul
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
  mul mulster $options(fn) $options(-mode) $options(-backup) \
    $options(-keep) $options(-single) $options(-charset) $options(-lineend)
  mul destroy
}

#############################################################################
#
# for "Run me" of e_menu:
#
#-ARGS0: -k 1 -mode glob test/test12ini
#-ARGS1: -b 0 tasks/mulster-tke
#-ARGS2: -b 0 tasks/mulster-geany
#ARGS3: -b 0 tasks/mulster-ruff
#
#############################################################################
