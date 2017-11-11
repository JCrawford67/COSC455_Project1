package edu.towson.cis.cosc455.jcrawf9.project1
//**********************************************************************************************************************
// Justin Crawford
// COSC 455
// Project 1 - GITTEX
//**********************************************************************************************************************
import scala.collection.mutable.Stack
import java.awt.Desktop
import java.io._

class MySemanticAnalyzer {
  var html : String = ""
  var stack = Compiler.Parser.stack
  var docStack = Stack[String]()
  var tempStack = Stack[String]()
  var varStack = Stack[String]()
  var boldTag : Int = 0
  var italTag : Int = 0
  var focus : String = ""

  // This method gets the parse tree and iterates through it and converts each token to HTML code
  def convertToHTML(): Unit = {

    while(!stack.isEmpty){
      focus = stack.pop()
      getHTML()
    }
    html = docStack.mkString
    writeToFile(Compiler.fileName)
    openHTMLFileInBrowser(Compiler.fileName)
  }

  // This method matches GITTEX tags to its respective HTML tag or calls a select method to process it
  def getHTML(): Unit = {

    focus.toUpperCase() match{
      case CONSTANTS.DOCB => docStack.push(CONSTANTS.HTMLOPEN)
      case CONSTANTS.PARAB => docStack.push(CONSTANTS.POPEN)
      case CONSTANTS.PARAE => docStack.push(CONSTANTS.PCLOSE)
      case CONSTANTS.BRACKETE => handleBracTag()
      case CONSTANTS.ADDRESSE => handleImgLink()
      case CONSTANTS.BOLD => handleBold()
      case CONSTANTS.ITALICS => handleItalics()
      case CONSTANTS.NEWLINE => docStack.push(CONSTANTS.BR)
      case CONSTANTS.DOCE => docStack.push(CONSTANTS.HTMLCLOSE,CONSTANTS.BODYCLOSE)
      case "\n" => handleHeadList()
      case _ => docStack.push(focus)
    }

  }

  // This method decides whether use of the parenthesis tags will result in a link or an image
  def handleImgLink(): Unit = {
    while(!stack.top.equals(CONSTANTS.LINKB) && !stack.top.equals(CONSTANTS.IMAGEB)){
      tempStack.push(stack.pop())
    }
    val rootTag = stack.pop()
    if(rootTag.equals(CONSTANTS.LINKB)) handleLink()
    else handleImg()
  }

  // This method handles instances of links if found
  def handleLink(): Unit = {
    var link : String = ""
    var linkText : String = ""
    var linkTag : String = ""
    tempStack = tempStack.reverse
    while(!tempStack.top.equals(CONSTANTS.ADDRESSB)){
      link = tempStack.pop() + link
    }
    tempStack.pop()
    while(!tempStack.isEmpty){
      if(!tempStack.top.equals("]")) linkText = tempStack.pop() +linkText
      else tempStack.pop()
    }
    linkTag = "<a href=\"" + link + "\">"+linkText+"</a>"
    docStack.push(linkTag)
  }

  // This method handles instances of images if found
  def handleImg(): Unit = {
    var link : String = ""
    var altText : String = ""
    var imgTag : String = ""
    tempStack = tempStack.reverse
    while(!tempStack.top.equals(CONSTANTS.ADDRESSB)){
      link = tempStack.pop() + link
    }
    tempStack.pop()
    while(!tempStack.isEmpty){
      if(!tempStack.top.equals("]")) altText = tempStack.pop() + altText
      else tempStack.pop()
    }
    imgTag = "<img src=\"" + link + "\" alt=\"" + altText +"\">"
    docStack.push(imgTag)
  }

  // This method decides whether or not use of newLine Chars will result in use of heading or list item
  def handleHeadList(): Unit = {

    while(!stack.top.equals("#") && !stack.top.equals("+")){
      tempStack.push(stack.pop())
    }
    val rootTag : String = stack.pop()
    if(rootTag.equals("#")) handleHead()
    else handleList()

  }

  // This method handles instances of headings if found
  def handleHead(): Unit = {
    var heading : String = ""
    while(!tempStack.isEmpty) heading += tempStack.pop()
    docStack.push(CONSTANTS.H1CLOSE,heading,CONSTANTS.H1OPEN)
  }

  // This method handles instances of a list item if found
  def handleList(): Unit = {
    docStack.push(CONSTANTS.LICLOSE)
    tempStack = tempStack.reverse

    while(!tempStack.isEmpty){
      val temp : String = tempStack.pop()
      if(temp.equals(CONSTANTS.BOLD)) handleBold()
      else if(temp.equals(CONSTANTS.ITALICS)) handleItalics()
      else if(temp.equals(CONSTANTS.ADDRESSE)){
        var link : String = ""
        var linkText : String = ""
        var linkTag : String = ""
        while(!tempStack.top.equals(CONSTANTS.ADDRESSB)){
          link = tempStack.pop() + link
        }
        tempStack.pop()
        while(!tempStack.top.equals(CONSTANTS.LINKB)){
          if(!tempStack.top.equals("]")) linkText = tempStack.pop() +linkText
          else tempStack.pop()
        }
        tempStack.pop()
        linkTag = "<a href=\"" + link + "\">"+linkText+"</a>"
        docStack.push(linkTag)
      }
      else if(temp.equals(CONSTANTS.BRACKETE)) handleVarUse()
      else docStack.push(temp)
    }
    docStack.push(CONSTANTS.LIOPEN)
  }

  // This method handles use of variables by calling the findVar() method
  def handleVarUse(): Unit = {
    val varName = tempStack.pop()
    docStack.push(findVar(varName))
    if(!tempStack.isEmpty) tempStack.pop()

  }

  // This method handles instances of variable definitions
  // VarDefs don't make it into HTML code so I'm leaving this method blank so as to ignore them
  def handleVarDef(): Unit = {
    // Whole lotta nothin...
  }

  /*
  *  This method searches for a variable in the stack and checks that it is defined using the right scope.
  *  The method also checks if the expected value is appropriate based on scope. The method assumes that
  *  the variable has already been declared previously and ignores any paragraph blocks that it encounters
  *  while going back.
  */
  def findVar(varName : String): String = {
    var varDef : String = ""
    var found = false
    var inScope = true

    while(!found && !stack.isEmpty){
      if(stack.top.equalsIgnoreCase(CONSTANTS.PARAE)) inScope = false
      if(stack.top.equalsIgnoreCase(CONSTANTS.PARAB)) inScope = true
      if(stack.top.equalsIgnoreCase(CONSTANTS.DEFB)) {
        if (varStack.top.equals(varName) && inScope){

          stack.push(varStack.pop())
          stack.push(varStack.pop())
          varDef = varStack.top
          found = true
        }
        varStack.push(stack.pop())
      }
      else varStack.push(stack.pop())
    }
    if(varDef.equals("")) SemanticError("Variable "+ varName +" not defined for this scope.")

    while(!varStack.isEmpty){
      stack.push(varStack.pop())
    }
    varDef
  }

  // This method handles the use of italics if encountered
  // The counter used is there so determine if the found tag is an opening or closing tag
  def handleItalics(): Unit = {
    if(italTag == 0){
      docStack.push(CONSTANTS.ICLOSE)
      italTag = 1
    }
    else{
      docStack.push(CONSTANTS.IOPEN)
      italTag = 0
    }
  }

  // This method handles the use of bold tags if encountered
  // The counter used is there so determine if the found tag is an opening or closing tag
  def handleBold(): Unit = {
    if(boldTag == 0){
      docStack.push(CONSTANTS.BCLOSE)
      boldTag = 1
    }
    else{
      docStack.push(CONSTANTS.BOPEN)
      boldTag = 0
    }

  }

  // This method determines whether an instance of a right bracket is for a title or a variable definition
  def handleBracTag(): Unit = {
    while(stack.top.charAt(0) != '\\'){
      tempStack.push(stack.pop())
    }
    val rootTag : String = stack.pop()
    rootTag.toUpperCase() match{
      case CONSTANTS.TITLEB => handleTitle()
      case CONSTANTS.DEFB => handleVarDef()
      case CONSTANTS.USEB =>handleVarUse()
    }
  }

  // This method handles instances of titles
  def handleTitle(): Unit = {
    var title : String = ""
    while(!tempStack.isEmpty){
      title += tempStack.pop()
    }
    docStack.push(CONSTANTS.BODYOPEN, CONSTANTS.TITLECLOSE, title, CONSTANTS.TITLEOPEN)
  }

  // This method writes the generated HTML code to an HTML file
  def writeToFile(name : String): Unit = {
    val file = new File(name)
    val wr = new BufferedWriter(new FileWriter(file))
    wr.write(html)
    wr.close()
  }

  // This method is essentially a hack utilizing both Scala and Java functions
  // This method takes the name of the generated HTML file and opens it in your default browser
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }

  // This method returns an error message if a semantic error is encountered
  def SemanticError(error : String): Unit = {
    println("Semantic error discovered. \n" + error + "\nExiting.")
    System.exit(1)
  }

}
