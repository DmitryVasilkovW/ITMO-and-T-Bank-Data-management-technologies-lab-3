package building

import building.Building._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BuildingSpec extends AnyFlatSpec with Matchers {

  val maleResident: Resident = Resident(40, Male)
  val femaleResident: Resident = Resident(35, Female)
  val floorA: ResidentialFloor = ResidentialFloor(maleResident, femaleResident, null)
  val floorB: ResidentialFloor = ResidentialFloor(maleResident, Resident(30, Female), null)
  val commercialFloorA: CommercialFloor = CommercialFloor(List(Commercial("Restaurant")), null)
  val atticA: Attic = CommonAttic
  val buildingWithMultipleResidents: Building = Building("789 Park Ave", floorA)
  val buildingWithShops: Building = Building("101 Pine St", commercialFloorA)
  val simpleAtticBuilding: Building = Building("202 Cedar St", atticA)

  "Resident" should "throw exception if age is non-positive" in {
    assertThrows[IllegalArgumentException] {
      Resident(0, Male)
    }
  }

  it should "throw exception if gender is non-binary" in {
    assertThrows[IllegalArgumentException] {
      Resident(45, null.asInstanceOf[Gender])
    }
  }

  "countOldManFloors" should "return correct count of old men floors based on age" in {
    countOldManFloors(buildingWithMultipleResidents, 35) shouldEqual 1
  }

  it should "return 0 when no men are older than the specified age" in {
    countOldManFloors(buildingWithMultipleResidents, 45) shouldEqual 0
  }

  it should "return 0 when there are no men in the building" in {
    countOldManFloors(simpleAtticBuilding, 35) shouldEqual 0
  }

  "womanMaxAge" should "return the age of the oldest woman in the building" in {
    womanMaxAge(buildingWithMultipleResidents) shouldEqual Some(35)
  }

  it should "return None when there are no women in the building" in {
    womanMaxAge(simpleAtticBuilding) shouldEqual None
  }

  "countCommercial" should "return the number of commercial establishments" in {
    countCommercial(buildingWithShops) shouldEqual 1
  }

  it should "return 0 when there are no commercial establishments" in {
    countCommercial(simpleAtticBuilding) shouldEqual 0
  }

  "countCommercialAvg" should "return the average number of commercial establishments across multiple buildings" in {
    countCommercialAvg(List(buildingWithShops, simpleAtticBuilding)) shouldEqual 0.5
  }

  it should "return 0 when there are no commercial establishments in the buildings" in {
    countCommercialAvg(List(simpleAtticBuilding)) shouldEqual 0.0
  }

  it should "return 0 when no buildings are provided" in {
    countCommercialAvg(List.empty[Building]) shouldEqual 0.0
  }

  "evenFloorsMenAvg" should "return the average count of men on even floors" in {
    val buildingWithMultipleFloors =
      Building("Even Towers", ResidentialFloor(maleResident, femaleResident, commercialFloorA))
    evenFloorsMenAvg(buildingWithMultipleFloors) shouldEqual 1.0
  }

  it should "return 0 when there are no men in the building" in {
    evenFloorsMenAvg(simpleAtticBuilding) shouldEqual 0.0
  }
}
