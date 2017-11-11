package edu.towson.cis.cosc455.jcrawf9.project1
//**********************************************************************************************************************
// Justin Crawford
// COSC 455
// Project 1
//**********************************************************************************************************************
import scala.collection.mutable.Stack

class MySyntaxAnalyzer extends SyntaxAnalyzer{

  var stack  = Stack[String]()
  val scanner = Compiler.Scanner

  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      // add to parse tree / stack
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Error")
      System.exit(1)
    }
  }

  // This method implements the BNF rule for a paragraph
  override def paragraph(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      parse()
      varCheck()
      innerText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)){
        parse()
      }
      else SyntaxError("Expected Paragraph Closing tag in place of " + Compiler.currentToken)
    }
  }

  // This method implements the BNF rule for an Inner Item
  override def innerItem(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      variableUse()
      innerItem()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold()
      innerItem()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)){
      italics()
      innerItem()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      link()
      innerItem()
    }
    if(isText()){
      reqText()
    }
  }

  // This method implements the BNF rule for Inner Text
  override def innerText(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      variableUse()
      innerText()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)){
      heading()
      innerText()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold()
      innerText()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)){
      italics()
      innerText()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)){
      listItem()
      innerText()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      image()
      innerText()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      link()
      innerText()
    }
    if(isText()){
      parse()
      innerText()
    }
  }

  // This method implements the BNF rule for a Link
  override def link(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      parse()
      reqText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        parse()
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
          parse()
          reqText()
          if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
            parse
          }
          else SyntaxError("Expected closing link tag in place of " + Compiler.currentToken)
        }
        else SyntaxError("Expected opening address tag in place of " + Compiler.currentToken)
      }
      else SyntaxError("Expected right bracket tag in place of " + Compiler.currentToken)
    }
    else SyntaxError("Expected opening link tag in place of " + Compiler.currentToken)
  }

  // This method implements the BNF rule for Italics
  override def italics(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)){
      parse()
      getText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)){
        parse()
      }
      else SyntaxError("Expected closing italics tag in place of " + Compiler.currentToken)
    }
  }

  // This method implements the BNF rule for the body
  override def body(): Unit = {
    if(CONSTANTS.innerText.exists(x => x.equalsIgnoreCase(Compiler.currentToken))){
      innerText()
      body()
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      paragraph()
      body()
    }
    if(Compiler.currentToken.equals(CONSTANTS.NEWLINE)){
      newline()
      body()
    }
  }

  // This method implements the BNF rule for Bold text
  override def bold(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      parse()
      getText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
        parse()
      }
      else SyntaxError("Expected closing bold tag in place of " + Compiler.currentToken)
    }
  }

  // This method implements the BNF rule for a Newline
  override def newline(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      parse()
    }
  }

  // This method implements the BNF rule for the title
  override def title(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      parse()
      reqText()
      if(Compiler.currentToken.equals(CONSTANTS.BRACKETE)){
        parse()
      }
      else SyntaxError("Expect closing title tag in place of " + Compiler.currentToken)
    }
    else SyntaxError("Expected " + CONSTANTS.TITLEB +" in place of " + Compiler.currentToken)
  }

  // This method implements the BNF rule for Defining a Variable
  override def variableDefine(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      parse()
      addVar()
      if(Compiler.currentToken.equals(CONSTANTS.EQSIGN)){
        parse()
      }
      else SyntaxError("Expected " + CONSTANTS.EQSIGN + " in place of " +Compiler.currentToken)
      addVar()
      if(Compiler.currentToken.equals(CONSTANTS.BRACKETE)){
        parse()
      }
      else SyntaxError("Expected closing variable definition tag in place of " +Compiler.currentToken)
    }
  }

  // This method implements the BNF rule for an Image
  override def image(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      parse()
      reqText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        parse()
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
          parse()
          reqText()
          if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
            parse
          }
          else SyntaxError("Expected closing image tag in place of " + Compiler.currentToken)
        }
        else SyntaxError("Expected opening address tag in place of " + Compiler.currentToken)
      }
      else SyntaxError("Expected right bracket tag in place of " + Compiler.currentToken)
    }
    else SyntaxError("Expected opening image tag in place of " + Compiler.currentToken)
  }

  // This method implements the BNF rule for Using a Variable
  override def variableUse(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      parse()
      addVar()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        parse()
      }
      else SyntaxError("Expected closing variable use tag in place of " + Compiler.currentToken)
    }
  }

  // This method implements the BNF rule for a heading
  override def heading(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)){
      parse()
      reqText()
    }
  }

  // This method implements the BNF rule for a List Item
  override def listItem(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)){
      parse()
      innerItem()
      listItem()
    }
  }

  // ========== Helper Methods ==========

  // This method pushes a select token into the parse tree and gets next token
  def parse(): Unit ={
    stack.push(Compiler.currentToken)

    if(scanner.hasNextToken()){
      scanner.getNextToken()
    }
  }

  // This method returns an error if there is no text at a spot when text is expected
  def reqText(): Unit ={
    if(isText()){
      getText()
    }
    else SyntaxError("Text is required here. You didn't have any so you're seeing an error now.")
  }

  // This method adds text Chars to the parse tree
  def getText(): Unit ={
    while(isText()){
      parse()
    }
  }

  //This method collects all the Chars in a given variable definition/ variable use to make finding them in the parse tree easier
  def addVar(): Unit ={
    var token : String = ""
    while(isText()){

      if(!CONSTANTS.whiteSpace.exists(x => x.equals(Compiler.currentToken))){
        token += Compiler.currentToken
      }

      scanner.getNextToken()
    }
    stack.push(token)
  }

  //This method checks if select token is valid text
  def isText(): Boolean ={
    CONSTANTS.validText.exists(x => x.equalsIgnoreCase(Compiler.currentToken))
  }

  // This method helps the variableDefine method to make sure that only one variable is added at a time
  def varCheck(): Unit ={
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      variableDefine()
      varCheck()
    }
  }

  // This method reports a syntax error and exits the program
  def SyntaxError(error : String): Unit ={
    println("Syntax error discovered. \n" + error + "\nExiting.")
    System.exit(1)
  }
}
