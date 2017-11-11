package edu.towson.cis.cosc455.jcrawf9.project1
//**********************************************************************************************************************
// Justin Crawford
// COSC 455
// Project 1 - GITTEX
//**********************************************************************************************************************

object Compiler {

  var currentToken : String = ""
  var fileContents : String = ""
  var fileName : String = ""

  // Initialize the 3 components of a compiler
  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer

  def main(args: Array[String]) {
    // Check usage
    checkfile(args)
    readfile(args(0))
    fileName = mkFileName(args(0))

    // Gets all valid lexems and finds lexical errors
    Scanner.getLexems()

    // Get first token
    Scanner.getNextToken()

    // Calls start state of BNF in SyntaxAnalyzer
    Parser.gittex()

    /* On return there is a parse tree, SemanticAnalyzer
       Checks for Semantic errors while creating
       The HTML and then creates the HTML file. */
    SemanticAnalyzer.convertToHTML()
  }

  // Reads the file into a String to be processed
  def readfile(filename : String) ={
    val source = scala.io.Source.fromFile(filename)
    fileContents = try source.mkString finally source.close()

  }
  // Checks for correct number of arguments and for correct file type
  def checkfile(args : Array[String]) ={
    if(args.length != 1){
      println("Either no arguments or too many arguments. \nOne argument is required. \nExiting")
      System.exit(1)
    }
    else if(! args(0).endsWith(".gtx")){
      println("The file: " + args(0) + " does not have the correct file extension.\n*.mkd required. \nExiting ")
      System.exit(1)
    }

  // Creates the filepath for the finished HTML doc that was created
  }
  def mkFileName(fullPath : String): String ={
    fullPath.split('.').init ++ Seq("html") mkString "."
  }
}
