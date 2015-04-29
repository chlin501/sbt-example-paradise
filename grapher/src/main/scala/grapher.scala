package grapher

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object analyze {
  
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    //import Flag._
    
   @deprecated("","") def extractCaseClassesParts(classDecl: ClassDef) = {
     println(Console.BLUE_B + "in extractCaseClassesParts" + Console.RESET)
       classDecl match {
         case q"$mods class $className(..$fields) extends ..$parents { ..$body }" =>
           (className, fields, parents, body)
      } 
    }
    
   @deprecated("","") def modifiedDeclaration(classDecl: ClassDef) = {
      println(Console.BLUE_B + "in modified declaration" + Console.RESET)
      val (className, fields, parents, body) = extractCaseClassesParts(classDecl)

      //println(Console.BLUE_B + s"before returning (fields are $params)" + Console.RESET)
      val params = fields.asInstanceOf[List[ValDef]] map { p => p.duplicate}
      println(Console.BLUE_B + s"before returning" + Console.RESET)
      
      c.Expr[Any](
        q"""
        case class $className ( ..$params ) extends ..$parents {
          ..$body
        }
      """
      )
    }
    
    if (annottees.length != 1) { // I have yet to have seen this happen or know what syntax of annotation would allow that
      print(Console.YELLOW)
      println("annottees length: " + annottees.length)
      println("annottes: " + annottees)
      print(Console.RESET)
    }
    
    val returnIdentity = c.Expr[Any](Block(annottees.map(_.tree).toList, Literal(Constant(()))))
    
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
    
    println(Console.GREEN + "about to typecheck" + Console.RESET)
    val typeCheckedAnnottees = annottees.map(annottee => c.typecheck(annottee.tree, silent = false)).toList // this doesn't throw, but it will cause the macro to crash upon its return!
    println(Console.GREEN_B + "typechecking didn't crash!" + Console.RESET)
   
    //annottees.map(_.tree).toList match {
    typeCheckedAnnottees match {
      
      case x@q"$mods object $name extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil =>
        println(s"found object $name")
        findMethods("object", name, body)
        c.Expr[Any](Block(annottees.map(_.tree).toList, Literal(Constant(()))))
        
      //case (classDecl: ClassDef) :: Nil => modifiedDeclaration(classDecl)        
        
      case x@q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil => 
        println(s"found class $name")
        findMethods("class", name, body)
        returnIdentity
        
          
      case x@q"import $ref.{..$sels}" => 
        println(s"found import $ref $sels")
        returnIdentity
        
      case x => 
        println(Console.YELLOW + "in unmatched AST part" + Console.RESET); q"..$x"
        returnIdentity
    }
    
    //c.Expr[Any](Block(annottees.map(_.tree).toList, Literal(Constant(())))) // return expression that duplicates the original annottees (from https://github.com/scalamacros/paradise/blob/5e5f0c129dd1861f86250d7ce94635b89996938c/tests/src/main/scala/identity.scala#L8)
   
    //annottees.head
    //c.Expr[Any](result)
  }
}

class NN extends StaticAnnotation {
  def macroTransform(annottees: Any*) : Any = macro analyze.impl
}
