package edu.towson.cis.cosc455.jcrawf9.project1
//**********************************************************************************************************************
// Justin Crawford
// COSC 455
// Project 1 - GITTEX
//**********************************************************************************************************************

trait LexicalAnalyzer {
  def addChar(): Unit

  def getChar(): Char

  def getNextToken(): Unit
  /*
  *  Since this is the Scala equivalent of an interface, i elected to keep it
  *  it completely abstract by implementing lookup() within the lexical analyzer class
  */
  def lookup(): Boolean
}

