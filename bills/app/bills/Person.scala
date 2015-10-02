package bills

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.PoisonPill
import akka.actor.Props

import scala.language.postfixOps

object SnakeConfig {
  val marchTimeSeconds = 1
}

case class addPerson(person: Person)
case class changeSum(person: Person, sum: Int)

object PersonActor {
  def props(person : Person): Props = 
  {
    Props(new PersonActor(person))
  }
}

class PersonActor(person : Person) extends Actor {
  var person_ = person
  def receive = {
    case addPerson(person: Person) => 
    {
      person_.addPerson(person)
    }
    case changeSum(person: Person, sum: Int) => 
    {
      person_.changeSum(person, sum)
    }
  }
}

class Person(room: Room, name: String, id: Int)
{
  var room_ = room
  var name_ = name
  var id_ = id
  var neighbors_ = collection.mutable.Map[Person, Int]()
  def addPerson(person: Person)
  {
      if(!neighbors_.contains(person))
        neighbors_ += (person -> 0)
  }
  override def equals(o: Any) = o match {
    case that: Person => (that.id_ == this.id_) 
    case _ => false
  }
  def changeSum(person: Person, sum: Int)
  {
      person match {
        case CommonPerson =>
        {
          for(element <- neighbors_.keys)
          {
            neighbors_(element) += sum;
          }
        }
        case _ => 
        {
          neighbors_(person) += sum
        }
      }      
  }
}

object CommonPerson extends Person(new Room(RoomInfo(-1, -1), -1), "all", -1);
