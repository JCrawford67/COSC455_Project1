package edu.towson.cis.cosc455.jcrawf9.project1

trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Char
  def getNextToken() : Unit
  def lookup() : Boolean = {
    println("this is lookup implementation!")
  }
}
