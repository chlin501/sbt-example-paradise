import grapher.NN
import scala.concurrent.Future

object Test extends App {
}

abstract class logTag 
object Default     extends logTag 
object Performance extends logTag
object RDBMS       extends logTag

abstract class MessageType
object Normal extends MessageType
object Error extends MessageType

abstract class TagFilter { 
  def apply(msgTags: Seq[logTag]) : Seq[logTag]
}

object VoidTagFilter extends TagFilter {
  def apply(msgTags: Seq[logTag]) = msgTags
}

class ExclusiveTagFilter(excludeTags: Seq[logTag]) extends TagFilter { 
  def apply(msgTags: Seq[logTag]) = excludeTags.filterNot(tag => msgTags.exists(_ == tag))
} 

class InclusiveTagFilter(includeTags: Seq[logTag]) extends TagFilter {
  def apply(msgTags: Seq[logTag]) = includeTags.filter(tag => msgTags.exists(_ == tag)) 
} 

abstract class Expander {
  def apply(msg: String, msgTags: Seq[logTag], messageType: MessageType) : String
}

object Util {
  def getObjectName(obj: Object) = obj.getClass.toString.split('$').last.split('.').last // obj.getClass.getSimpleName.dropRight(1)
}; import Util._

case class A() {
  def doA = { println("A"); 3 }
}

@NN case class TestCaseClass2(foo: String)

@NN object DefaultExpander extends Expander {
  def foo = { val a = 3 }
  def apply(msg: String, tags: Seq[logTag], messageType: MessageType) : String = {
    import Console._
    foo
    val a = new A
    a.doA
    val b = new B
    b.doB
    val tagString = tags.map(tag => "[" + getObjectName(tag) + "]").mkString(" ")
    messageType match {
      case Normal => WHITE + tagString + RESET + " " + BLUE + BOLD + msg + RESET  
      case Error  => WHITE + tagString + " " + RESET + RED_B + WHITE + " Error " + RESET + " " + RED + msg + RESET // + " " + RED_B + WHITE + BOLD + " ERROR "
    }
  }
}

@NN object Foo 

abstract class UnderlyingExternalLogger {
  def apply(finalMessage: String, messageType: MessageType) 
}

class SLF4J(externalLogName: String) extends UnderlyingExternalLogger {
  val externalLogger = org.slf4j.LoggerFactory.getLogger(externalLogName)

  def apply(finalMessage: String, messageType: MessageType) = messageType match {
    case Normal => externalLogger.info(finalMessage)
    case Error  => externalLogger.error(finalMessage)
  }
} 

object DefaultUnderlyingExternalLogger extends SLF4J("articlio") 

object TestDefaultUnderlyingExternalLogger extends SLF4J("test") 
  
case class Logger(tagFilter: TagFilter, 
                  expander: Expander, 
                  underlyingExternalLogger: UnderlyingExternalLogger) {
  
  def log(msg: String, console: Boolean = false, messageType: MessageType = Normal)
         (implicit tags: Seq[logTag] = Seq(Default)): Boolean = {

    import Console._
    
    val commonTags = tagFilter(tags)
    messageType match {
      case Error  => 
        underlyingExternalLogger(expander(msg, tags, messageType), messageType)
        Console.println(RED + BOLD + "Error: " +  msg + RESET)
        true
      case Normal =>
        commonTags.nonEmpty match {
          case true => 
            underlyingExternalLogger(expander(msg, commonTags, messageType), messageType)
            console match {
              case true => Console.println(msg)
              case false =>
            }        
            true
          case false => false
        }
    }
  }
}

object A