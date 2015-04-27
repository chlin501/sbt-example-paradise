import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object analyze {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    
    def findTypeMethods(name: String, body: List[c.Tree]) = {
      body map {
        case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
          println(Console.BLUE + Console.BOLD + s"type $name has method $tname" + Console.RESET)
          expr.foreach { 
            case q"$f($args)" => println(s"  which calls $f with args $args")
            case _ =>
          } 
        case _ =>
      }     
    }
    
    val result = {
      annottees.map(_.tree).toList match {
        
        case x@q"$mods object $name extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil =>
          println(s"found object $name")
          findTypeMethods(name.toString, body)
          q"..$x"
          
        case x@q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil => 
          println(s"found class $name")
          findTypeMethods(name.toString, body)
          q"..$x"
          
        case x => q"..$x" 
          
      }
    }
    c.Expr[Any](result)
  }
}

class ג extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro analyze.impl
}
