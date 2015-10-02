package bills

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.PoisonPill
import akka.actor.Props

import scala.language.postfixOps

case class Event(actor: Person, subject: Person, sum: Int, text: String);
case class addEvent(element: Person, subject: Person, sum: Int, text: String);

case class RoomInfo(num : Int, dormitory : Int);


object RoomActor {
  def props(room : Room): Props = 
  {
    Props(new RoomActor(room))
  }
}

class RoomActor(room : Room)  extends Actor
{
  var room_ = room
  def receive = {
    case addPerson(person: Person) => {
      room_.addPerson(person)
      var person_actor = context.actorOf(PersonActor.props(person), "id" + person.id_.toString)
      var allPersons = context.actorSelection("/user/room" + room_.id_.toString + "/*")
      allPersons ! addPerson(person)
      for(i <- room_.members_)
      {
        person_actor ! addPerson(i)
      }
    }
    case addEvent(element: Person, CommonPerson, sum: Int, text: String) => {
      var element_actor = context.actorSelection("/user/room" + room_.id_.toString + "/id" + element.id_)
      var allPersons = context.actorSelection("/user/room" + room_.id_.toString + "/*")
      println("Hello world")
      element_actor ! new changeSum(CommonPerson, sum / (room_.members_.count(_ => true)))
      allPersons ! new changeSum(element, -sum / (room_.members_.count(_ => true)))
      room_.addEvent(Event(element, CommonPerson, sum, text))
    }
    case addEvent(element: Person, subject: Person, sum: Int, text: String) => {
      var element_actor = context.actorSelection("/user/room" + room_.id_.toString + "/id" + element.id_)
      var subject_actor = context.actorSelection("/user/room" + room_.id_.toString + "/id" + subject.id_)
      element_actor ! new changeSum(subject, sum)
      subject_actor ! new changeSum(element, -sum)
      println(element)
      println(subject)
      println(element.name_)
      println(subject.name_)
      room_.addEvent(Event(element, subject, sum, text))
    }
  }
}

class Room(roomInfo : RoomInfo, id : Int)
{
  var members_ = List[Person]()
  var roomInfo_ = roomInfo
  var history = new collection.mutable.Stack[Event]()
  var id_ = id
  def addPerson(person : Person)
  {
      members_ :::= List(person)
  }
  def addEvent(event : Event)
  {
      history.push(event)
  }
  def getMessages(person: Person):collection.mutable.Stack[String] = 
  {
    val Fix_Person = person
    val result = history.map
    {
      case Event(actor, CommonPerson, sum, text) =>
      {
        var noun = actor.name_
        if(actor == person)
          noun = "Я "
        noun + " купил " + text + " за " + sum.toString
      }
      case Event(actor, Fix_Person, sum, text) => 
      {
        var verb : String = ""
        if(sum > 0)
          verb = " дал мне "
        else
          verb = " взял у меня " 
        actor.name_ + verb + sum.toString + " \"" + text + "\""
      }
      case Event(Fix_Person, actor, sum, text) => 
      {
        var verb : String = ""
        if(sum > 0)
          verb = "Я дал "
        else
          verb = "Я взял у "
        verb + actor.name_ + " " + sum.toString + " \"" + text + "\""
      }
      case _ => ""
    }
    result
  }
}
