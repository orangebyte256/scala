package bills

import akka.actor.ActorSelection.toScala
import akka.actor.PoisonPill
import play.api.libs.iteratee.Concurrent
import play.api.libs.iteratee.Iteratee
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.WebSocket
import play.libs.Akka
import play.api.libs.Crypto
import snakeyard.actor.WebSocketChannel
import snakeyard.actor.SnakePool
import snakeyard.actor.NewSnake
import snakeyard.actor.ChangeDirection
import scala.concurrent.ExecutionContext.Implicits.global

import java.io._
import scala.io.Source
import java.nio.file.{Paths, Files}

object Global
{
  val hostIP = "192.168.0.7:9000"
  var encryptKey : String = "unset"
  val fileUsersName = "users.txt"
}

object Manager
{
  var rooms = collection.mutable.Map[RoomInfo, Room]()
  var users = collection.mutable.Map[Int, Person]()
  var passwords = collection.mutable.Map[String, String]()
  var names = collection.mutable.Map[String, Int]()
  var id = 0

  def getRoomNum(dormitory : Int, room : Int) : Int = 
  {
      dormitory * 1009 + room
  }

  def getRoomFileHistory(dormitory : Int, room : Int) : String = 
  {
      return (getRoomNum(dormitory, room)).toString + "_history.txt"
  }

  def addPersonToBase(dormitory : Int, room : Int, name : String, username : String, password : String)
  {    
      println(room + dormitory)
      if(!rooms.contains(RoomInfo(room, dormitory)))
      {
        println("i'm here")
        var room_object = new Room(RoomInfo(room, dormitory), getRoomNum(dormitory, room))
        var room_actor = Akka.system.actorOf(RoomActor.props(room_object), "room" + getRoomNum(dormitory, room).toString)
        rooms += (RoomInfo(room, dormitory) -> room_object)
      }
      println("error314")
      var room_object = rooms.get(RoomInfo(room, dormitory)).get
      var room_actor = Akka.system.actorSelection("/user/room" + getRoomNum(dormitory, room).toString)
      var person_object = new Person(room_object, name, id)
      users += (id -> person_object)
      room_actor ! addPerson(person_object)
      passwords += (username -> password)
      names += (username -> id)
      id = id + 1
  }

  def addEventAction(actor_id : Int, subject_id : Int, get_sum : Int, type_message : String, message : String)
  {
      var person = users.get(actor_id).get
      var subject : Person = CommonPerson
      var sum = get_sum
      if(type_message != "common")
      {
        subject = users.get(subject_id).get
      }
      if(type_message == "get")
      {
        sum = -sum;
      }
      var room_actor = Akka.system.actorSelection("/user/room" + getRoomNum(person.room_.roomInfo_.dormitory, person.room_.roomInfo_.num).toString)
      room_actor ! addEvent(person, subject, sum, message)  
  }

}

object BillsController extends Controller {
  for (line <- Source.fromFile(Global.fileUsersName, "utf-8").getLines()) {
    val arr = line.split(" ")
    val name = arr.slice(4, arr.size)
    Manager.addPersonToBase(arr(0).toInt, arr(1).toInt, name.mkString(" "), arr(2), arr(3))
  }

  for(element <- Manager.rooms.keys)
  {
    val filename = Manager.getRoomFileHistory(element.dormitory,element.num)
    if(Files.exists(Paths.get(filename)))
    {
      println("error1")
      for (line <- Source.fromFile(filename, "utf-8").getLines()) 
      {
        println("Hellllllllllllllll1")
        val arr = line.split(" ")
        println(arr)
        val message = arr.slice(4, arr.size)
        println(message)
        println("Hellllllllllllllll2")
        Manager.addEventAction(arr(0).toInt, arr(1).toInt, arr(2).toInt, arr(3), message.mkString(" "))
      }
    }  
  }
  println("error2")
  def login(reply: String = "") = Action
  {
      Ok(views.html.login(reply)).withNewSession
  }

  def loginValidate = Action 
  {
    implicit request =>
    var name = request.getQueryString("username").get
    var password = request.getQueryString("password").get
    if ((!Manager.passwords.contains(name)) || (Manager.passwords.get(name).get != Crypto.encryptAES(password, Global.encryptKey)))
    {
      Redirect("/loginerror")
    }
    else
    {
      var id = Manager.names.get(name).get
      var person = Manager.users.get(id)
      Redirect("/").withSession(
        "user" -> id.toString)
//      Ok(views.html.index(person.get)).withSession(
//        "user" -> id.toString)
    }
  }

  def setEncryptKey(reply: String) = Action
  {
    if(Global.encryptKey == "unset")
    {
      Global.encryptKey = reply
      Ok("Успешно, Дима!")
    }
    else
    {
      Ok("Хуй тебе, мразь!")
    }
  }

  def register(reply: String = "") = Action
  {
      Ok(views.html.register(reply))
  }

  def okey() = Action
  {
    implicit request =>
    Ok(views.html.register(""))
  }

  def registerValidate = Action 
  {
    implicit request =>
    var username = request.getQueryString("username").get
    println(username)
    if(Manager.names.contains(username))
    {
      Redirect("/registerreply")
    }
    else
    {
      var room = request.getQueryString("room").get.toInt
      var dormitory = request.getQueryString("dormitory").get.toInt
      println("gui")
      println(room)
      var name = request.getQueryString("name").get
      var password = request.getQueryString("password").get
      password = Crypto.encryptAES(password, Global.encryptKey)
      Manager.addPersonToBase(dormitory, room, name, username, password)
      var out = new BufferedWriter(new OutputStreamWriter(
      new FileOutputStream(Global.fileUsersName,true), "UTF-8"));
      try {
          out.write(dormitory + " " + room + " " + username + " " + password + " " + name);
          out.newLine();
      } finally {
          out.close();
      }
      Redirect("/login_")
    }
  }

  def index = Action { implicit request =>
    {
      var tmp_id = request.session.get("user")
      if (tmp_id == None)
      {
        Ok(views.html.login.render(""))
      }
      else
      {
        var person = Manager.users.get(tmp_id.get.toInt)
        Ok(views.html.index(person.get))
      }
    }
  }

  def addEventAction = Action { implicit request =>
    {
      var actor = request.session.get("user").get.toInt
      var person = Manager.users.get(actor).get
      var type_message = request.getQueryString("type").get
      var subject = -1
      if(type_message != "common")
      {
        subject = request.getQueryString("subject").get.toInt
      }
      var sum = request.getQueryString("sum").get.toInt
      var message = request.getQueryString("message").get
      Manager.addEventAction(actor, subject, sum, type_message, message)
      var out = new BufferedWriter(new OutputStreamWriter(
      new FileOutputStream(Manager.getRoomFileHistory(person.room_.roomInfo_.dormitory, person.room_.roomInfo_.num),true), "UTF-8"));
      try {
          out.write(actor.toString + " " + subject.toString + " " + sum.toString + " " + type_message + " " + message);
          out.newLine();
      } finally {
          out.close();
      }
      Redirect("/")
    }
  }
  

  def toInt(s: Option[String]): Int = {
    val intValue = try {
      Some(s.get.toInt)
    } catch {
      case e: Exception => None
    }
    intValue.getOrElse(-1)
  }

}
