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
#       -exact 0 | 1
#       -backup 0 | <dir>
#       -keep 0 | 1
#       --
# See README for details.
#############################################################################

oo::class create Mulster {

  variable _mulster

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  constructor {args} {
    array set _mulster {}
    set _mulster(IN_FILE)   INFILE=   ;# pattern for in file name
    set _mulster(OUT_FILE)  OUTFILE=  ;# pattern for out file name
    set _mulster(IN_BEGIN)  IN=BEGIN  ;# pattern for in lines start
    set _mulster(IN_END)    IN=END    ;# pattern for in lines finish
    set _mulster(OUT_BEGIN) OUT=BEGIN ;# pattern for out lines start
    set _mulster(OUT_END)   OUT=END   ;# pattern for out lines finish
    set _mulster(EXACT)     EXACT=    ;# pattern for exact mode
    set _mulster(BACKUP)    BACKUP=   ;# pattern for backup dir name
    set _mulster(KEEP)      KEEP=     ;# pattern for keep mode
    set _mulster(GRP1) {(.+)}           ;# RE group for a file name
    set _mulster(GRP2) {\((.+),(.+)\)}  ;# RE groups for range
    if { [self next] != {} } {
      return [next {*}$args]
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Processes files according to the options of 'fileini' file.
  # Input parameters:
  #   'fileini' is options file name
  #   'exact' if 1, means in-lines should be matched exact, with
  #           all their leading/tailing spaces
  #   'backup' if 0, means no backuping, otherwise - backup directory
  #   'keep' if 1, keeps input files' attributes/times in output files

  method mulster {fileini {exact 0} {backup BAK} {keep 0}} {
    if {!($exact in {0 1})} {set exact 1}
    if {!($keep in {0 1})} {set keep 1}
    my FlushOut -1 ;# just to initialize
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
          if [my OptionIs $_mulster(EXACT) $_mulster(GRP1)] {
            my FlushOut $exact $keep
            set exact $_mulster(match1)
            if {!($exact in {0 1})} {set exact 1}
          # check for backup dir
          } elseif [my OptionIs $_mulster(BACKUP) $_mulster(GRP1)] {
            my FlushOut $exact $keep
            set backup $_mulster(match1)
          # check for exact mode
          } elseif [my OptionIs $_mulster(KEEP) $_mulster(GRP1)] {
            my FlushOut $exact $keep
            set keep $_mulster(match1)
            if {!($keep in {0 1})} {set keep 1}
          # check for input file
          } elseif [my OptionIs $_mulster(IN_FILE) $_mulster(GRP1)] {
            my FlushOut $exact $keep
            set _mulster(fin) 1
            set _mulster(infile) $_mulster(match1)
            my BackupFile $backup $_mulster(infile)
          # check for output file
          } elseif [my OptionIs $_mulster(OUT_FILE) $_mulster(GRP1)] {
            my CheckForError 1
            set _mulster(fout) 1
            set _mulster(outfile) $_mulster(match1)
          # check for input lines beginning
          } elseif {[my OptionIs $_mulster(IN_BEGIN)] || \
                    [my OptionIs $_mulster(IN_BEGIN) $_mulster(GRP2)]} {
            my CheckForError 1
            my CheckForError 2
            lappend _mulster(inlist) [list $_mulster(match1) $_mulster(match2)]
            set mode IN
          # check for output lines beginning
          } elseif [my OptionIs $_mulster(OUT_BEGIN)] {
            my CheckForError 1
            my CheckForError 2
            lappend _mulster(outlist) [list]
            set mode OUT
          }
        }
        IN {
          # check for input lines ending
          if [my OptionIs $_mulster(IN_END)] {
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
          if [my OptionIs $_mulster(OUT_END)] {
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
    my FlushOut $exact $keep  ;# flush out the collected lines if any
    close $chini
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Performs replacements in a list ('lcont') according to a list of
  # input lines ('lin') and  a list of output lines ('lout'), in
  # a range of replacements set with 'r1' and 'r2' parameters.
  # If 'exact' is true, the lines are compared to be strongly equal,
  # otherwise their leading/tailing spaces are ignored at comparing.

  method mulster1 {lcont lin lout {r1 0} {r2 0} {exact 1}} {
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

  method FlushOut {exact {keep 1}} {
    if {$exact!=-1 && ($_mulster(fin) + $_mulster(fout))} {
      my CheckForError 1
      my CheckForError 2
      my Flush1 $exact $keep
    }
    set _mulster(fin) 0            ;# flag of "input file defined"
    set _mulster(fout) 0           ;# flag of "output file defined"
    set _mulster(inlist) [list]    ;# in lines list
    set _mulster(outlist) [list]   ;# out lines list
  }

  method Flush1 {exact keep} {
    puts "MULSTER: $_mulster(infile) ==> $_mulster(outfile)"
    if {$keep} {
      lassign [my FileAttributes $_mulster(infile)] attrs atime mtime
    }
    set ch [open $_mulster(infile)]
    set lcont [split [read $ch] \n]
    close $ch
    for {set il 0} {$il<[llength $_mulster(inlist)]} {incr il} {
      set lin [lindex $_mulster(inlist) $il]
      set lout [lindex $_mulster(outlist) $il]
      set r1 [lindex $lin 0]
      set r2 [lindex $lin 1]
      set lcont [my mulster1 $lcont [lrange $lin 2 end] $lout $r1 $r2 $exact]
      puts -nonewline "Replacement #[expr {$il+1}]: "
      if {$_mulster(repls)==0} {
        puts "NOTHING CHANGED!"
      } else {
        puts "$_mulster(repls) done"
      }
    }
    set ch [open $_mulster(outfile) w]
    foreach stout $lcont {
      if {[incr iamidiotofFMD]>1} {puts $ch ""}
      puts -nonewline $ch $stout
    }
    close $ch
    if {$keep} {
      my FileAttributes $_mulster(outfile) $attrs $atime $mtime
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

  method BackupFile {backup filename} {
    puts ""
    if {$backup!="0" && $backup!=""} {
      lassign [my FileAttributes $filename] attrs atime mtime
      set ch [open $filename]
      set cont [read $ch]
      close $ch
      catch {file mkdir $backup}
      set bakfile [file join $backup [file tail $filename]]
      set ch [open $bakfile w]
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
       -exact 0 | 1
       -backup 0 | <dir>
       -keep 0 | 1
       --

 See README for details.
"
    exit
  }
  array set options {-exact 0 -backup BAK -keep 0 fn {}}
  set off 0
  foreach {opt val} $::argv {
    if {$off} {
      set options(fn) [string trim "$options(fn) $opt $val"]
      continue
    }
    switch -exact $opt {
      -e - -exact  { set options(-exact) $val }
      -b - -backup { set options(-backup) $val }
      -k - -keep   { set options(-keep) $val }
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
#############################################################################
# for "Run me" of TKE editor's e_menu plugin:
#-ARGS0: -k 1 test/test12ini
#-ARGS1: -e 1 -b 0 tasks/mulster-tke
#ARGS2: -e 1 -b 0 tasks/mulster-geany
  Mulster create mul
  mul mulster $options(fn) $options(-exact) $options(-backup) $options(-keep)
  mul destroy
#############################################################################
}

