package Test

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation


object analyze {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    
    println(annottees.length)
    println(annottees)
    
    def findTypeMethods(name: String, body: List[c.Tree]) = {
      body map {
        case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
          println(Console.BLUE + Console.BOLD + s"type $name has method $tname" + Console.RESET)
          expr.foreach { 
            case q"$obj $f" => println(s"  which calls " + Console.BLUE + Console.BOLD + f + Console.RESET + 
                                        " on object " + obj + 
                                        " of type " + Console.CYAN_B + obj.tpe.typeSymbol + Console.RESET)
                               //if (obj.tpe != null) println(Console.CYAN_B + obj.tpe.typeSymbol + Console.RESET)
            //case q"$obj $f($args)" => println(s"  which calls $f with args $args")
            case x => println(Console.YELLOW + s"in unmatched AST part: $x" + Console.RESET); 
          } 
        case x => println(Console.YELLOW + s"in unmatched AST part: $x" + Console.RESET);  
      }     
    }
    
    val result = {
      annottees.map(a => c.typecheck(a.tree, silent = true)).toList match {
        
        case x@q"$mods object $name extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil =>
          println(s"found object $name")
          findTypeMethods(name.toString, body)
          q"..$x"
          
        case x@q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil => 
          println(s"found class $name")
          println(Console.YELLOW + x + Console.RESET);
          findTypeMethods(name.toString, body)
          q"..$x"
          
        case x@q"import $ref.{..$sels}" => println(s"found import $ref $sels")
          q"..$x"
          
        case x => println(Console.YELLOW + "in unmatched AST part" + Console.RESET); q"..$x" 
          
      }
    }
    c.Expr[Any](result)
    //annottees.toList
  }
}

class ג extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro analyze.impl
}
