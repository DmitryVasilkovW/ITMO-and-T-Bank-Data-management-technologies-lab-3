package building

import scala.annotation.tailrec

sealed trait Gender
case object Male extends Gender
case object Female extends Gender

case class Resident(age: Int, gender: Gender) {
  require(age > 0, "Age must be positive")
  require(gender == Male || gender == Female, "gender must be binary")
}

case class Commercial(name: String)

sealed trait Floor
case class CommercialFloor(establishments: List[Commercial], next: Floor) extends Floor
case class ResidentialFloor(resident1: Resident, resident2: Resident, next: Floor) extends Floor
sealed trait Attic extends Floor
case object CommonAttic extends Attic
case class CommercialAttic(establishment: Commercial) extends Attic

case class Building(address: String, firstFloor: Floor)

object Building {

  def apply(address: String, floors: List[Floor]): Either[String, Building] = {
    if (floors.isEmpty) Left("Building must have at least one floor.")
    else Right(Building(address, floors.head))
  }

  private def fold[T](building: Building, accumulator: T)(f: (T, Floor) => T): T = {
    @tailrec
    def loop(floor: Floor, acc: T): T = {
      if (floor == null) {
        acc
      } else {
        floor match {
          case _: Attic                     => acc
          case CommercialFloor(_, next)     => loop(next, f(acc, floor))
          case ResidentialFloor(_, _, next) => loop(next, f(acc, floor))
        }
      }
    }
    loop(building.firstFloor, accumulator)
  }

  def countOldManFloors(building: Building, olderThan: Int): Int = {
    fold(building, 0) {
      case (acc, ResidentialFloor(Resident(age, Male), _, _)) if age > olderThan => acc + 1
      case (acc, ResidentialFloor(_, Resident(age, Male), _)) if age > olderThan => acc + 1
      case (acc, _)                                                              => acc
    }
  }

  def womanMaxAge(building: Building): Option[Int] = {
    fold(building, Option.empty[Int]) {
      case (acc, ResidentialFloor(Resident(age, Female), _, _)) => Some(acc.fold(age)(math.max(_, age)))
      case (acc, ResidentialFloor(_, Resident(age, Female), _)) => Some(acc.fold(age)(math.max(_, age)))
      case (acc, _)                                             => acc
    }
  }

  def countCommercial(building: Building): Int = {
    fold(building, 0) {
      case (acc, CommercialAttic(_))                 => acc + 1
      case (acc, CommercialFloor(establishments, _)) => acc + establishments.length
      case (acc, _)                                  => acc
    }
  }

  def countCommercialAvg(buildings: List[Building]): Double = {
    if (buildings.isEmpty) {
      0.0
    } else {
      val (total, count) = buildings.foldLeft((0, 0)) { (acc, building) =>
        val commercialCount = countCommercial(building)
        (acc._1 + commercialCount, acc._2 + 1)
      }
      total.toDouble / count
    }
  }

  def evenFloorsMenAvg(building: Building): Double = {
    val (totalMen, count) = fold(building, (0, 0)) {
      case ((total, cnt), ResidentialFloor(Resident(_, Male), _, next)) =>
        (total + 1, cnt + 1)
      case ((total, cnt), ResidentialFloor(_, Resident(_, Male), next)) =>
        (total + 1, cnt + 1)
      case ((total, cnt), ResidentialFloor(Resident(_, Male), Resident(_, Male), next)) =>
        (total + 2, cnt + 1)
      case ((total, cnt), _) =>
        (total, cnt)
    }

    if (count == 0) {
      0.0
    } else {
      totalMen.toDouble / count
    }
  }
}
