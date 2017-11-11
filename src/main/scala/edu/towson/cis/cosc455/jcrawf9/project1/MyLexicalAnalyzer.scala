package edu.towson.cis.cosc455.jcrawf9.project1
//**********************************************************************************************************************
// Justin Crawford
// COSC 455
// Project 1 - GITTEX
//**********************************************************************************************************************
import scala.collection.mutable.ArrayBuffer

class MyLexicalAnalyzer extends LexicalAnalyzer {

  var position : Int = 0
  var nextChar : Char = 0
  var currentToken : String = ""
  var lexems : ArrayBuffer[String] = new ArrayBuffer[String]()
  var itr : Iterator[String] = null
  val terminals = CONSTANTS.terminals

  // This method uses the iterator for the lexemes array to return any token that has yet to be processed
  override def getNextToken(): Unit = {
    Compiler.currentToken = itr.next()
  }

  // This method returns true if there is more than one token
  def hasNextToken(): Boolean = {
    itr.hasNext
  }

  // This method gets all of the lexems in the file and initializes the lexems iterator
  def getLexems(): Unit = {
    while(position < Compiler.fileContents.length()){
      nextChar = getChar()
      getToken()
    }
    itr = lexems.iterator
  }

  // This method processes tokens and separates the special Chars and the normal text
  def getToken(): Unit ={
    if(isSpecial()){
      addChar()
      specialToken(nextChar)
    }
    else if(isText()){
      addText()
    }
    else if(nextChar == "") {}
    else{
      invalidTokenError(nextChar+"3")
    }
  }

  // This method uses pattern matching to decide how to process a special character
  def specialToken(c : Char): Unit = {
    c match{
      case CONSTANTS.BSLASH => slashTag()
      case CONSTANTS.LEFTBR => leftBrack()
      case CONSTANTS.EXCLAI => exclaim()
      case CONSTANTS.HASHTG => headingList()
      case CONSTANTS.PLUSIG => headingList()
      case CONSTANTS.ASTERI => styleTags()
    }
  }

  // This method is a helper method for specialToken() that handles all the special tags
  def slashTag(): Unit ={
    nextChar = getChar()
    // If the next Char is a new line, add it to the list.
    if(nextChar == CONSTANTS.BSLASH){
      addChar()
      addToken()
    }
    else{
      // Continue adding characters to token until new line or left bracket is encountered
      while(nextChar != '\n' && nextChar != '['){
        if(isAlphaText()) addChar()
        else if(isSpace()) {} //Do Nothing
        else invalidTokenError("'"+nextChar+"' 1");
        nextChar = getChar()
      }
      if(nextChar == '[') addChar()
      // Token should be valid now but throw an error if not
      if(!lookup()) invalidTokenError(currentToken+"2")
      addToken()
      // If innerPiece is encountered, then add innerText in between brackets
      if(lexems.last.equalsIgnoreCase(CONSTANTS.TITLEB) || lexems.last.equalsIgnoreCase(CONSTANTS.USEB) ||
        lexems.last.equalsIgnoreCase(CONSTANTS.DEFB)){
        nextChar = getChar()
        while(nextChar != ']'){
          addInnerText()
          nextChar = getChar()
        }
        if(nextChar == ']') addText()
      }
    }
  }

  // If left bracket is encountered first, then get link token
  def leftBrack(): Unit ={
    addText()
    nextChar = getChar()
    addLink()
  }

  // If exclamation point is encountered before left bracket, then add image tag, else throw an error
  def exclaim(): Unit ={
    nextChar = getChar()
    if(nextChar != '[') invalidTokenError(nextChar+"")
    addChar()
    addToken()
    nextChar = getChar()
    addLink()
  }

  // If pound sign (#) is encountered, then create a list
  // Add text until new line is encountered
  def headingList(): Unit ={
    addText()
    nextChar = getChar()
    while(nextChar != '\n'){
      getToken()
      nextChar = getChar()
    }
    addLine()
  }

  // If a star (*) is encountered, then either bold or italics are created
  // This method will look ahead to check for a closing star Char. If none then move position back
  def styleTags(): Unit ={

    currentToken = nextChar+""
    nextChar = getChar()
    if(nextChar == '*'){
      currentToken += '*'
      addToken()
    }
    else{
      addToken()
      position = position-1

    }
  }

  //========== HELPER METHODS ==========

  // This method gets the next Char from file
  override def getChar(): Char = {
    val c: Char = Compiler.fileContents.charAt(position)
    position += 1
    c
  }

  // This method adds Char from nextChar to token
  override def addChar(): Unit = {
    currentToken += nextChar
  }

  // This method adds current token to lexems list and resets the token
  def addToken(): Unit ={
    lexems += currentToken
    resetToken()
  }

  // This method adds a single token to the lexems list. Newline Chars are ignored for this method
  def addText(): Unit ={
    if(nextChar != '\n' && nextChar != '\t'){
      resetToken()
      addChar()
      addToken()
    }
  }

  // This method adds a line Char to lexems list. Used for headings and lists
  def addLine(): Unit ={
    if(nextChar == '\n'){
      resetToken()
      addChar()
      addToken()
    }
  }

  // This method adds text to the variable definition tokens.
  // This method allows use of equals Char
  def addInnerText(): Unit ={
    if(isText() || CONSTANTS.EQSIGN.equals("=")){
      addText()
    }
  }

  // This method adds a link to the lexems list.
  def addLink(): Unit ={
    while(nextChar != ']'){
      addText()
      nextChar = getChar()
    }
    if(nextChar == ']') addText()
    nextChar = getChar()
    removeSpace()
    if(nextChar == '(') addText()
    nextChar = getChar()
    while(nextChar != ')'){
      addText()
      nextChar = getChar()
    }

    addText()
  }

  // This method skips over space Chars when encountered
  def removeSpace(): Unit ={
    while(isSpace()){
      nextChar = getChar()
    }
  }

  // This method resets the current token to Nil
  def resetToken(): Unit ={
    currentToken = ""
  }

  // This method returns true if current Char is valid text
  def isText(): Boolean = {
    CONSTANTS.validText.exists(x => x.equalsIgnoreCase(nextChar + ""))
  }

  // This method returns true if current Char is a valid letter Char (alpha-text)
  def isAlphaText(): Boolean = {
    CONSTANTS.letters.exists(x => x.equalsIgnoreCase(nextChar + ""))
  }

  // This method returns true if current Char is a valid special Char
  def isSpecial(): Boolean = {
    CONSTANTS.specialChar.exists(x => x.equals(nextChar + ""))
  }

  // This method returns true if current Char is white-space text
  def isSpace(): Boolean = {
    CONSTANTS.whiteSpace.exists(x => x.equals(nextChar + ""))
  }

  // This method returns true if current Char returns true if current token is valid
  override def lookup(): Boolean = {
    terminals.exists(x => currentToken.equalsIgnoreCase(x))
  }

  // This method returns an error message if a lexical error is encountered
  def invalidTokenError(token:String) = {
    println("Lexical error discovered at character: " + position + "\n" + token + " is not a valid symbol.\nExiting.")
    System.exit(1)
  }
}
