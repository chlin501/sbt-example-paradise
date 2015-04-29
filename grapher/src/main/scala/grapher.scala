package grapher

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

class NN extends StaticAnnotation {
  def macroTransform(annottees: Any*) : Any = macro analyze.impl
}

object analyze {
  
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    //import Flag._
    import Console._
    
   @deprecated("","") def extractCaseClassesParts(classDecl: ClassDef) = {
     println(BLUE_B + "in extractCaseClassesParts" + RESET)
       classDecl match {
         case q"$mods class $className(..$fields) extends ..$parents { ..$body }" =>
           (className, fields, parents, body)
      } 
    }
    
   @deprecated("","") def modifiedDeclaration(classDecl: ClassDef) = {
      println(BLUE_B + "in modified declaration" + RESET)
      val (className, fields, parents, body) = extractCaseClassesParts(classDecl)

      //println(BLUE_B + s"before returning (fields are $params)" + RESET)
      val params = fields.asInstanceOf[List[ValDef]] map { p => p.duplicate}
      println(BLUE_B + s"before returning" + RESET)
      
      c.Expr[Any](
        q"""
        case class $className ( ..$params ) extends ..$parents {
          ..$body
        }
      """
      )
    }
    
    if (annottees.length != 1) { // I have yet to have seen this happen or know what syntax of annotation would allow that
      print(YELLOW)
      println("annottees length: " + annottees.length)
      println("annottes: " + annottees)
      print(RESET)
    }
    
    val returnIdentity = c.Expr[Any](Block(annottees.map(_.tree).toList, Literal(Constant(())))) // return expression that duplicates the original annottees (from https://github.com/scalamacros/paradise/blob/5e5f0c129dd1861f86250d7ce94635b89996938c/tests/src/main/scala/identity.scala#L8)
    
    def findMethods(typ: String, name: Any, body: List[c.Tree]) = {
      body map {
        case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
          println(CYAN_B + s"$typ $name" + RESET + 
                  " has method " + BLUE + BOLD + 
                  tname + RESET)
          
          expr.foreach { 
            // TODO: need to further hone the quasiquote for capturing only and all cases of method application, *along* the object they are applied to.
            case q"$obj $f" => println(s"  which calls " + BLUE + BOLD + f + RESET + 
                                        " on object " + obj + 
                                        " of type " + CYAN_B + obj.tpe.typeSymbol + RESET)
                                        // if (obj.tpe != null) println(CYAN_B + obj.tpe.typeSymbol + RESET)
            case x => // println(YELLOW + s"in unmatched AST part: $x" + RESET); 
            //case q"$obj $f($args)" => println(s"  which calls $f with args $args")
          } 
        case x => // println(YELLOW + s"in unmatched AST part: $x" + RESET);  
      }     
    }
    
    println(GREEN + "about to typecheck" + RESET)
    val typeCheckedAnnottees = annottees.map(annottee => c.typecheck(annottee.tree, silent = false)).toList // this doesn't throw, but it will cause compilation to abort after all annotations were handled
    println(GREEN_B + "typechecking didn't crash!" + RESET)
   
    //annottees.map(_.tree).toList match {
    typeCheckedAnnottees match {
      
      case x@q"$mods object $name extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil =>
        println(s"found object $name")
        findMethods("object", name, body)
        returnIdentity
        
      //case (classDecl: ClassDef) :: Nil => modifiedDeclaration(classDecl)        
        
      case x@q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$body }" :: Nil => 
        println(s"found class $name")
        findMethods("class", name, body)
        returnIdentity
        
          
      case x@q"import $ref.{..$sels}" => 
        println(s"found import $ref $sels")
        returnIdentity
        
      case x => 
        println(YELLOW + "in unmatched AST part" + RESET); q"..$x"
        returnIdentity
    }
    
    //annottees.head
    //c.Expr[Any](result)
  }
}
