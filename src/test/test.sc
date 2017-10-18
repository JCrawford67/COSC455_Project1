// infix
3 + 4

//prefix
!true

//scala vars are mutable (can be changed)

var a : Int = 5

var sum : Int = 10 + a

a=10

//scala vals are immutable (cant be changed)

val b : Int = 15

//b = 5  //this is not legal

//expression oriented statment in Scala:

var c = if (a < b) 25 else 30

//statement oriented (like in Java)

if (a < b)
  c = 25
else
  c = 30

//arguments

var args : Array[String] = new Array[String] (1)
args(0) = "myfile.gtx"

//useful in Proj 1
if(args.length != 1) {
  println("USAGE ERROR: wrong number of args")
  //System.exit(1)
}
else if (args(0).endsWith(".gtx")) {
  println("USAGE ERROR: wrong file type")
  //System.exit(1)
}

//Scala selection example using case/switch

var chr : Char = '%'

def caseSwitch (c : Char) = {
   match {
    case '\\' => println("case 1: begin, end use, def, title")
    case '#' => println("case 2: heading")
    case '*' => println("case 3: bold or italic")
    case '+' => println("case 4: list item")
    case '[' => println("case 5: link")
    case '!' => println("case 6: image")
    case default => println("ERROR! Unexpected character")
  }
}

caseSwitch(chr)

//Scala iteration

var l = 0

for (a <- 1 to 10){ //can also go from 10 to 1
  println("Value of a: " + a) // var a becomes the control variable
}

//parse tree

var parseTree = new scala.collection.mutable.Stack[String]

parseTree.push("\\BEGIN")
parseTree.push("\\TITLE[MyPage]")
parseTree.push("\\This is a simple Gittex document")
parseTree.push("\\END")

parseTree.reverse.foreach()

// constructed / user-defined types (this is dumb)

type SuperString = String

var justin : SuperString = "Justin"

//Smart constructed types (smarter thing to do)

type IntList = List[Int]

// new types

type fahrenheit = Int
type celcius = Int

var var1 : fahrenheit = 10
var var2 : celcius = var1

var1 + var2
