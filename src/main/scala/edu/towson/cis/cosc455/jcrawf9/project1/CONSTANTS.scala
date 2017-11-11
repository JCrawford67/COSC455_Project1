package edu.towson.cis.cosc455.jcrawf9.project1
//*********************************************************************************************************************
// Justin Crawford
// COSC 455
// Project 1 - GITTEX
//**********************************************************************************************************************

object CONSTANTS {
  // Terminal constants
  val DOCB : String = 	"\\BEGIN"
  val DOCE : String = 	"\\END"
  val TITLEB : String = "\\TITLE["
  val BRACKETE : String = "]"
  val HEADING : String = "#"
  val PARAB : String = "\\PARAB"
  val PARAE : String = "\\PARAE"
  val BOLD : String = "**"
  val ITALICS : String = "*"
  val LISTITEM : String = "+"
  val NEWLINE  : String = "\\\\"
  val LINKB : String = "["
  val ADDRESSB : String = "("
  val ADDRESSE : String = ")"
  val IMAGEB : String = "!["
  val DEFB : String = "\\DEF["
  val EQSIGN : String = "="
  val USEB : String = "\\USE["

  // Special Characters
  val BSLASH : Char = '\\'
  val LEFTBR : Char = '['
  val EXCLAI : Char = '!'
  val HASHTG : Char = '#'
  val PLUSIG : Char = '+'
  val ASTERI : Char = '*'

  //Handy lists - honestly a lot of them are unnecessary but at this point I'm too scared to touch anything here.
  val specialChar : List[String] = List("\\","[","#","*","!","+")

  val letters : List[String] = List("a","b","c","d","e","f","g","h","i","j","k","l","m",
    "n","o","p","q","r","s","t","u","v","w","x","y","z")
  val numbersEtc : List[String] = List("1","2","3","4","5","6","7","8","9","0",
    ",",".","\"",":","?","_","/", "'", "")
  val whiteSpace : List[String] = List(" ", "\t", "\n", "\b","\f","\r")
  val validText : List[String] = whiteSpace ::: letters ::: numbersEtc

  //val beginTag = List(DOCB, TITLEB, HEADING, PARAB, LISTITEM, LINKB, IMAGEB, DEFB,
    //USEB)
  //val endTag = List(DOCE, BRACKETE, PARAE, "\n")
  //val singleTag = List(NEWLINE)
  //val symetricTag = List(BOLD, ITALICS)
  //val addressContainer = List(ADDRESSB, ADDRESSE)
  val terminals = /*beginTag ::: endTag ::: singleTag ::: symetricTag ::: addressContainer :::*/ specialChar ::: validText

  val innerText = List(USEB, HEADING, BOLD, ITALICS, LISTITEM, IMAGEB, LINKB) ::: validText

  //HTML Tags
  val HTMLOPEN : String = "<HTML>"
  val HTMLCLOSE : String = "</HTML>"
  val HEADOPEN : String = "<HEAD>"
  val HEADCLOSE : String = "</HEAD>"
  val TITLEOPEN : String = "<TITLE>"
  val TITLECLOSE : String = "</TITLE>"
  val BODYOPEN : String = "<BODY>"
  val BODYCLOSE : String = "</BODY>"
  val POPEN : String = "<P>"
  val PCLOSE : String = "</P>"
  val H1OPEN : String = "<H1>"
  val H1CLOSE : String = "</H1>"
  val LIOPEN : String = "<LI>"
  val LICLOSE : String = "</LI>"
  val BOPEN : String = "<B>"
  val BCLOSE : String = "</B>"
  val IOPEN : String ="<I>"
  val ICLOSE : String = "</I>"
  val BR : String = "<BR />"
}
