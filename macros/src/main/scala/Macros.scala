package Test

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation


object analyze {
  
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    
    if (annottees.length != 1) { // I have yet to have seen this happen or know what syntax of annotation would allow that
      print(Console.YELLOW)
      println("annottees length: " + annottees.length)
      println("annottes: " + annottees)
      print(Console.RESET)
    }
    
    def findMethods(typ: String, name: Any, body: List[c.Tree]) = {
      body map {
        case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
          println(Console.CYAN_B + s"$typ $name" + Console.RESET + 
                  " has method " + Console.BLUE + Console.BOLD + 
                  tname + Console.RESET)
          
          expr.foreach { 
            case q"$obj $f" => println(s"  which calls " + Console.BLUE + Console.BOLD + f + Console.RESET + 
                                        " on object " + obj + 
                                        " of type " + Console.CYAN_B + obj.tpe.typeSymbol + Console.RESET)
                                        // if (obj.tpe != null) println(Console.CYAN_B + obj.tpe.typeSymbol + Console.RESET)
            case x => // println(Console.YELLOW + s"in unmatched AST part: $x" + Console.RESET); 
            //case q"$obj $f($args)" => println(s"  which calls $f with args $args")
          } 
        case x => // println(Console.YELLOW + s"in unmatched AST part: $x" + Console.RESET);  
      }     
    }
    
    val a: List[c.Tree] = annottees.map(a => c.typecheck(a.tree, silent = false)).toList
    
    annottees.map(a => c.typecheck(a.tree, silent = true)).toList match {
      
      case x@q"$mods object $name extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil =>
        println(s"found object $name")
        findMethods("object", name, body)
        
      case x@q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil => 
        println(s"found class $name")
        findMethods("class", name, body)
        
      case x@q"import $ref.{..$sels}" => println(s"found import $ref $sels")
        
      case x => // println(Console.YELLOW + "in unmatched AST part" + Console.RESET); q"..$x"
      
    }
    
    val result = c.Expr[Any](Block(annottees.map(_.tree).toList, Literal(Constant(())))) // return expression that duplicates the original annottees (from https://github.com/scalamacros/paradise/blob/5e5f0c129dd1861f86250d7ce94635b89996938c/tests/src/main/scala/identity.scala#L8)
    result
    //c.Expr[Any](result)
  }
}

class ג extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro analyze.impl
}
