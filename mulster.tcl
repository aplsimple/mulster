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
#       -exact 0 | 1
#       -backup 0 | <dir>
#       -keep 0 | 1
#       -single 0 | 1
#       -charset charsetName
#       -lineend lineEnding
#       --
# See README for details.
#############################################################################

oo::class create Mulster {

  variable _mulster

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  constructor {infile outfile args} {
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
    set _mulster(EXACT)     EXACT=    ;# pattern for exact mode
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

  method InitIO {} {

    # Initializes input & output files processing

    set _mulster(fin) 0            ;# flag of "input file defined"
    set _mulster(fout) 0           ;# flag of "output file defined"
    set _mulster(inlist) [list]    ;# in lines list
    set _mulster(outlist) [list]   ;# out lines list
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Processes files according to the options of 'fileini' file.
  # Input parameters:
  #   'fileini' is options file name
  #   'exact' if 1, means in-lines should be matched exact, with
  #           all their leading/tailing spaces
  #   'backup' if 0, means no backuping, otherwise - backup directory
  #   'keep' if 1, keeps input files' attributes/times in output files
  #   'single' if 1, standard 'one string for one string' replacements
  #   'charset' sets a charset of input files (e.g. cp1251)
  #   'lineend' sets a line ending of input files (e.g. \r\n)

  method mulster {fileini {exact 0} {backup BAK} {keep 0} \
  {single 0} {charset ""} {lineend ""}} {
    if {!($exact in {0 1})} {set exact 1}
    if {!($keep in {0 1})} {set keep 1}
    if {!($single in {0 1})} {set single 1}
    if {!($_mulster(fin) + $_mulster(fout))} { my InitIO }
    set mode NONE
    set _mulster(st) ""
    set _mulster(nl) 0
    set _mulster(fileini) $fileini
    set chini [open $fileini]
    foreach _mulster(st) [split [read $chini] \n] {
      incr _mulster(nl)
      switch $mode {
        NONE {
          # check for exact mode
          if {[my OptionIs $_mulster(EXACT) $_mulster(GRP1)]} {
            my FlushOut $exact $keep $charset $lineend $backup
            set exact $_mulster(match1)
            if {!($exact in {0 1})} {set exact 1}
          # check for backup dir
          } elseif {[my OptionIs $_mulster(BACKUP) $_mulster(GRP1)]} {
            my FlushOut $exact $keep $charset $lineend $backup
            set backup $_mulster(match1)
          # check for keep mode
          } elseif {[my OptionIs $_mulster(KEEP) $_mulster(GRP1)]} {
            my FlushOut $exact $keep $charset $lineend $backup
            set keep $_mulster(match1)
            if {!($keep in {0 1})} {set keep 1}
          # check for charset
          } elseif {[my OptionIs $_mulster(CHARSET) $_mulster(GRP1)]} {
            my FlushOut $exact $keep $charset $lineend $backup
            set charset $_mulster(match1)
          # check for lineend
          } elseif {[my OptionIs $_mulster(LINEEND) $_mulster(GRP1)]} {
            my FlushOut $exact $keep $charset $lineend $backup
            set lineend $_mulster(match1)
          # check for single mode
          } elseif {[my OptionIs $_mulster(SINGLE) $_mulster(GRP1)]} {
            set single $_mulster(match1)
            if {!($single in {0 1})} {set single 1}
          # check for input file
          } elseif {[my OptionIs $_mulster(IN_FILE) $_mulster(GRP1)]} {
            my FlushOut $exact $keep $charset $lineend $backup
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
            lappend _mulster(inlist) [list \
              $_mulster(match1) $_mulster(match2) $single $charset $lineend]
            set mode IN
          # check for output lines beginning
          } elseif {[my OptionIs $_mulster(OUT_BEGIN)]} {
            my CheckForError 1
            my CheckForError 2
            lappend _mulster(outlist) [list]
            set mode OUT
          }
        }
        IN {
          # check for input lines ending
          if {[my OptionIs $_mulster(IN_END)]} {
            set mode NONE
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
            set mode NONE
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
    my FlushOut $exact $keep $charset $lineend $backup
    close $chini
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Performs replacements in a list ('lcont') according to a list of
  # input lines ('lin') and  a list of output lines ('lout'), in
  # a range of replacements set with 'r1' and 'r2' parameters.
  #
  # If 'exact' is true, the lines are compared to be strongly equal,
  # otherwise their leading/tailing spaces are ignored at comparing.
  #
  # If 'single' is true, the input lines ('lin') are replaced with output
  # lines ('lout') in a list ('lcont') as standard string replacements.
  #
  # The 'charset' sets an encoding of input files (by default utf-8).
  # The 'lineend' sets an line ending of input files (by default \n).

  method mulster1 {lcont lin lout {r1 0} {r2 0} {exact 1} \
  {single 0} {charset ""} {lineend ""}} {
    set _mulster(repls) 0
    set leni [llength $lin]
    if {!$leni} {
      return $lcont  ;# nothing to replace
    }
    set lres [list]  ;# resulting list
    set sti0 [lindex $lin 0]
    if {!$exact} {set sti0 [string trim $sti0]}
    set leno [llength $lout]
    set lenc [llength $lcont]
    set ic [set ifnd 0]
    while {$ic < $lenc} {
      set stc [lindex $lcont $ic]
      if {[set stc [lindex $lcont $ic]] == $sti0 || \
      !$exact && [string trim $stc] == $sti0 } { ;# 1st line found
        for {set ii [set found 1]} {$ii<$leni && $found} {incr ii} {
          set ic2 [expr {$ic + $ii}]
          set st1 [lindex $lin $ii]
          set st2 [lindex $lcont $ic2]
          if {!$exact} {
            set st1 [string trim $st1]
            set st2 [string trim $st2]
          }
          set found [expr {$st1==$st2 && $ic2<$lenc}]
        }
        if {$found} {
          incr ifnd
          ;# check a found ifnd-th bunch for the range (r1,r2)
          if {$ifnd>=$r1 && ($ifnd<=$r2 || !$r2)} {
            for {set io 0} {$io<$leno} {incr io} {
              lappend lres [lindex $lout $io]
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

  method FlushOut {exact keep charset lineend backup} {
    if {$_mulster(fin) || $_mulster(fout)} {
      my CheckForError 1
      my CheckForError 2
      set root [file dirname $_mulster(infile)]
      set patt [file tail $_mulster(infile)]
      my recurseProc $root $root $patt $_mulster(outfile) \
        $exact $keep $charset $lineend $backup
    }
    my InitIO
  }

  method recurseProc {root dirname inpatt outpatt exact \
   keep charset lineend backup} {
    # Scans reversively a directory for files as glob patterns.
    #   root - a root directory name
    #   dirname - current directory name
    #   inpatt - glob pattern of input files
    #   outpatt - glob pattern of output files
    #   exact, keep, charset, lineend, backup - arguments of mulster
    #
    # For each found file, gets an output file name, makes an output
    # directory and calls the mulstering procedure.

    if {[string first * $outpatt]<0} {
      ;# no patterns, simple file-to-file processing
      my ProcFile [file join $root $inpatt] $outpatt \
        $exact $keep $charset $lineend $backup
      return
    }
    foreach dir [glob -nocomplain [file join $dirname *]] {
      if {[file isdirectory $dir]} {
        my recurseProc $root $dir $inpatt $outpatt \
          $exact $keep $charset $lineend $backup
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
          my ProcFile $f $outf $exact $keep $charset $lineend $backup
        }
      }
    }

  }

  method ProcFile {infile outfile exact keep charset lineend backup} {
    # Mulsters an input file to make an output file.
    #   infile -
    #   outfile -
    #   exact, keep, charset, lineend, backup - arguments of mulster

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
      lassign $lin r1 r2 single charset lineend
      set lcont [my mulster1 $lcont [lrange $lin 5 end] \
        $lout $r1 $r2 $exact $single $charset $lineend]
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
       -exact 0 | 1
       -backup 0 | <dir>
       -keep 0 | 1
       -single 0 | 1
       -charset charsetName
       -lineend lineEnding
       --

 See README for details.
"
    exit
  }
  array set options {-exact 0 -backup BAK -keep 0 -single 0 \
    -charset {} -lineend {} fn {}}
  set infile [set outfile ""]
  set off 0
  foreach {opt val} $::argv {
    if {$off} {
      set options(fn) [string trim "$options(fn) $opt $val"]
      continue
    }
    switch -exact $opt {
      -e - -exact   { set options(-exact) $val }
      -b - -backup  { set options(-backup) $val }
      -k - -keep    { set options(-keep) $val }
      -s - -single  { set options(-single) $val }
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
  Mulster create mul $infile $outfile
  mul mulster $options(fn) $options(-exact) $options(-backup) \
    $options(-keep) $options(-single) $options(-charset) $options(-lineend)
  mul destroy
}

#############################################################################
# for "Run me" of e_menu:
#-ARGS0: -k 1 test/test12ini
#-ARGS1: -e 1 -b 0 tasks/mulster-tke
#-ARGS2: -e 1 -b 0 tasks/mulster-geany
#ARGS2: -e 1 -b 0 tasks/mulster-ruff
#############################################################################
