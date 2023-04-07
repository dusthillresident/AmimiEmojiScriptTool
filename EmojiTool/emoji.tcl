package require Tk

array set phoneticDictionary {
 you {y w }
 ceiling {s l ng}
 thomas {t m s}
 i'd {' d}
 i'm {' m}
 would've {w d v}
 won't {w nt}
 could've {k d v}
 can't {k nt}
 should've {sh d v}
 would {w d}
 could {c d}
 should {sh d}
 question {k w s ch n}
 don't {d nt}
 whole {h l}
 what {w t}
 where {w r}
 everywhere {' v r w r}
 anywhere {' n w r}
 somewhere {s m w r}
 nowhere {n w r}
 place {p l s}
 efficient {' f sh n t}
 why {w y}
 jungle {j ng g l}
 comes {k m z}
 does {d z}
 is {' z}
 was {w z}
 dino {d n w}
 through {th r w}
 alright {' l r t}
 right {r t}
 fight {f t}
 bright {b r t}
 might {m t}
 sight {s t}
 saw {s}
 new {n y w}
 feature {f ch r}
 future {f y ch r}
 features {f ch r s}
 of {' v}
 magic {m j k}
}

proc getPhoneticList {word} {
 global phoneticDictionary
 if {[info exists phoneticDictionary($word)]} {
  return $phoneticDictionary($word)
 }
 set out ""
 set current ""
 set word [string tolower $word]
 if {[string first [string range $word 0 0] "aeiou"] != -1} {
  lappend out '
 } elseif {[string first "qu" $word] == 0} {
  set word [string range $word 2 end]
  set out "k w"
 }
 for {set i 0} {$i<=[string length $word]} {incr i} {
  set c [string range $word $i $i]
  if {[string first $c "qwrtypsdfghjklzxcvbnm"] != -1} { 
   append current $c
  } else {
   if {[string length $current]} {
    for {set j 0} {$j<[string length $current]} {incr j 2} {
     set c2 [string range $current $j 1+$j]
     if {[lsearch -exact { ch ng sh th bl cl
                           cr nd ns nt pl pr
                           sb sd sh sn tr mn
                           ck ll ff dd pp } $c2 ] != -1} {
      lappend out $c2
     } else {
      lappend out [lindex [split $c2 ""] 0]
      incr j -1
     } ;#endif
    } ;#next
   } ;#endif
   set current ""
  } ;#endif
 } ;#next
 return $out
} ;#endproc

proc makeBlueRedSwappedPic {p} {
 set w [image width $p]
 set h [image height $p]
 set out [image create photo -width $w -height $h]
 for {set x 0} {$x<$w} {incr x} {
  for {set y 0} {$y<$h} {incr y} {
   set c [$p get $x $y]
   if {!($c eq {255 255 255})} {
    set c [lreverse $c]
    lset c 0 [expr {min([lindex $c 0]+64,255)}]
   }
   $out put [eval format "#%02x%02x%02x" $c] -to $x $y
  }
 }
 return $out
}

proc loadGlyphs {} {
 global glyphs glyphMaxW glyphMaxH
 set glyphMaxW 0
 set glyphMaxH 0
 foreach i [glob *.png] {
  set glyphName [lindex [split $i .] 0]
  set glyphs($glyphName) [image create photo -file $i]
  set glyphMaxW [expr {max([image width  $glyphs($glyphName)],$glyphMaxW)}]
  set glyphMaxH [expr {max([image height $glyphs($glyphName)],$glyphMaxH)}]
  set redFileName "red/$glyphName.png"
  if {[file exists $redFileName]} {
   set glyphs($glyphName.red) [image create photo -file $redFileName]
  } else {
   set glyphs($glyphName.red) [makeBlueRedSwappedPic $glyphs($glyphName)]
   $glyphs($glyphName.red) write $redFileName
  }
 } ;#next

 foreach {i j} {
  ;     semicolon
  /	slash
  .	dot
  ck	k
  c	k
  ll	l
  ff	f
  dd	d
  pp    p
  !	exclaim
  ?	question
  ,	comma
  /	slash
  \\	slash2
  '	eye
 } {
  set glyphs($i) $glyphs($j)
  if [catch {
   set glyphs($i.red) $glyphs($j.red)
  }] {
   puts "fuck"
  }
 }
 #set glyphs(/) $glyphs(slash)
 #set glyphs(.) $glyphs(dot)
 #set glyphs(ck) $glyphs(k)
 #set glyphs(c) $glyphs(k)
 #set glyphs(ll) $glyphs(l)
 #set glyphs(ff) $glyphs(f)
 #set glyphs(dd) $glyphs(d)
 #set glyphs(!) $glyphs(exclaim)
 #set glyphs(?) $glyphs(question)
 #set glyphs(,) $glyphs(comma)
 #set glyphs(/) $glyphs(slash)
 #set glyphs(\\) $glyphs(slash2)
 #set glyphs(') $glyphs(eye)

 #foreach {i j} [array get glyphs] {
 # if { -1 == [string first ".red" $i] && ![info exists glyphs($i.red)] } {
 #  puts "$i	$j"
 # }
 #}

 # foreach {i j} [array get glyphs] {  catch { set glyphs($i) $glyphs($i.red) } }

} ;#endproc

loadGlyphs

set glyphModifier ""

proc getGlyph {phoneticPart} {
 global glyphs glyphModifier
 if [info exists glyphs($phoneticPart$glyphModifier)] {
  return $glyphs($phoneticPart$glyphModifier)
 } else {
  puts "getGlyph: '$phoneticPart' is missing"
  return $glyphs(missing)
 }
}

proc renderWord {word} {
 global glyphs glyphMaxW glyphMaxH renderedWords glyphModifier
 if {[info exists renderedWords($word$glyphModifier)]} {
  return $renderedWords($word$glyphModifier)
 }
 set out [image create photo]
 set phoneticList [getPhoneticList $word]
 #$out put "#ffffff" -to 0 0 [expr { ( int(ceil([llength $phoneticList]/2.0)) )*$glyphMaxW+20}] [expr {$glyphMaxH*2}]
 set count 0
 set l [llength $phoneticList]
 set x 0
 set curWidth 0
 foreach i $phoneticList {
  set glyph [getGlyph $i]
  set curWidth [expr {max($curWidth,[image width $glyph])}]
  $out copy $glyph -to $x [expr {int($glyphMaxH*(($count & 1)+($count+1==$l && ($l&1))*0.5)) }]
  incr count
  if {!($count&1)} {
   incr x $curWidth
   set curWidth 0
  }
 }
 set renderedWords($word$glyphModifier) $out
 return $out
}

proc renderString {str} {
 global glyphs glyphMaxW glyphMaxH renderedWords
 set out [image create photo]
 set x 0
 foreach i [split $str ""] {
  set glyph [getGlyph $i]
  $out copy $glyph -to $x $glyphMaxH
  incr x [image width $glyph]
 }
 return $out
}

proc prepareInput {in} {
 #set in [split $in]
 set out ""
 for {set i 0} {$i<=[string length $in]} {incr i} {
  set c [string range $in $i $i]
  #puts "c is '$c'"
  if {$c eq "\n"} {
   #puts "blah"
   set c " _NEWLINE_ "
  } elseif {$c eq " "} {
   set c " _SPACE_ "
  } elseif {[string is punct $c] && !($c eq "'")} {
   incr i
   set cc [string range $in $i $i]
   while {[string is punct -strict $cc] && !($cc eq "'")} {
    append c $cc
    incr i
    set cc [string range $in $i $i]
   }
   incr i -1
   set c " $c "
  } ;#endif

  append out $c
 } ;#next
 #return $out
 return [split $out]
}

proc isWord {str} {
 set l [string length $str]
 for {set i 0} {$i<=$l} {incr i} {
  set c [string range $str $i $i]
  if {![string is alpha $c] && ([string is punct $c] && !($c eq "'"))} {
   return 0
  }
 }
 return 1
}

proc fixPhoto {p} {
 set fix [image create photo]
 $fix put "#ffffff" -to 0 0 [image width $p] [image height $p]
 $fix copy $p -to 0 0
 image delete $p
 return $fix
}

proc renderDocument {doc} {
 global glyphMaxW glyphMaxH glyphModifier
 set doc [prepareInput $doc]
 set out [image create photo]
 set x 0
 set y 0
 foreach i $doc {
  switch -exact $i {
   £RED {
    set glyphModifier ".red"
   }
   £BLUE {
    set glyphModifier ""
   }
   _NEWLINE_ {
    set x 0
    incr y [expr { ($glyphMaxH<<1)+($glyphMaxH>>1) }]
   }
   _SPACE_ {
    incr x 16
   }
   default {
    if [isWord $i] {
     set word [renderWord $i]
     $out copy $word -to $x $y
     incr x [image width $word]
    } else {
     set str [renderString $i]
     $out copy $str -to $x $y
     incr x [expr {[image width $str]+$glyphMaxW}]
     image delete $str
    }
   }
  } ;#endcase
 } ;#next
 return [fixPhoto $out]
} ;#endproc


set myinput {One two three four!
Denver, the last dinosaur. 
He's my friend and a whole lot more. 
Denver, the last dinosaur. 
Shows me a world i never saw before. 
From a hot hot jungle a long time ago,
comes a cool cool friend, my pal dino. Oh!
Denver, the last dinosaur. He's my friend and a whole lot more.
Denver, the last dinosaur.
Shows me a world i never saw before. 
Everywhere we go, we don't really care,
if people stop and stare at our pal dino.
From prehistory through the rock and roll spotlight, we got the friend who helps us make it through alright.
That's Denver, the last dinosaur. He's my friend and a whole lot more. 
Denver, the last dinosaur.
Shows me a world i never saw before.}

if {!($argv eq "")} {
 set myinput [lindex $argv 0]
}

pack [label .l -image [renderDocument $myinput]]
#pack [label .l -image [renderWord "test!w"]]
[lindex [lsearch -glob -inline [.l conf] "*-image*"] 4] write "../emoji_document_output.png"