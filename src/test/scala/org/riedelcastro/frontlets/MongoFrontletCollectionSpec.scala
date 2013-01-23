package org.riedelcastro.frontlets

import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers
import org.scalatest.mock.MockitoSugar
import com.mongodb._
import org.mockito.Mockito._

/**
 * @author Sebastian Riedel
 */
class MongoFrontletCollectionSpec extends FunSpec with MustMatchers with MockitoSugar {

  import FrontletSpec._
  import collection.JavaConversions._
  import MongoFrontletImplicits._

  describe("A MongoFrontletCollection") {
    it("should translate frontlet insert into a corresponding mongo dbo insert") {
      val mockColl = mock[DBCollection]
      val coll = new MongoFrontletCollection(mockColl, () => new Person)
      val person = new Person().age(36).address.create(_.street("Broadway").number(1))
      val addressDBO = new BasicDBObject(Map("street" -> "Broadway", "number" -> 1))
      val personDBO = new BasicDBObject(Map("age" -> 36, "address" -> addressDBO))
      coll += person
      verify(mockColl).insert(personDBO)
    }
    it("should translate frontlet updates into a corresponding mongo dbo updates") {
      val mockColl = mock[DBCollection]
      val coll = new MongoFrontletCollection(mockColl, () => new Person)
      coll.update(_.age(36), _.age.update(37), upsert = false, multi = false)
      val query = new BasicDBObject(Map("age" -> 36))
      val update = new BasicDBObject("$set", new BasicDBObject("age", 37))
      verify(mockColl).update(query, update, false, false)
    }

    it("should translate a frontlet query into a mongo query and convert the returned dbo into a frontlet") {
      val mockColl = mock[DBCollection]
      val mockCursor = mock[DBCursor]
      val coll = new MongoFrontletCollection(mockColl, () => new Person)
      val query = new BasicDBObject(Map("age" -> 36))
      val keys = new BasicDBObject(Map("firstName" -> 1))
      val personDBO = new BasicDBObject(Map("firstName" -> "Fidel"))

      when(mockCursor.next()).thenReturn(personDBO)
      when(mockColl.find(query, keys)).thenReturn(mockCursor)

      val result = coll.query(_.age(36), _.firstName.select).next()
      result.firstName() must be("Fidel")

    }
  }
}


class MongoFrontletConverterSpec extends FunSpec with MustMatchers with MockitoSugar {

  import FrontletSpec._
  import collection.JavaConversions._

  describe("A MongoFrontletConverter") {
    it("should convert a dbo into an equivalent frontlet, and back") {
      val addressDBO = new BasicDBObject(Map("street" -> "Broadway", "number" -> 1))
      val personDBO = new BasicDBObject(Map("age" -> 36, "address" -> addressDBO))
      val frontlet = MongoFrontletConverter.eagerFrontlet(personDBO, () => new Person)
      val expected = MongoFrontletConverter.toMongo(frontlet.asMap)
      personDBO must be(expected)
    }
    it("should convery a dbo lazily to a frontlet") {
      val addressDBO = mock[DBObject]
      //need two parameters to fool faulty type checker
      when(addressDBO.get("number")).thenReturn(new java.lang.Integer(2), new java.lang.Integer(2))
      when(addressDBO.containsField("number")).thenReturn(true)

      val frontlet = MongoFrontletConverter.lazyFrontlet(addressDBO, () => new Address)
      verify(addressDBO, never()).get("number")
      val number = frontlet.number()
      verify(addressDBO, times(1)).get("number")
      number must be(2)
    }
  }
}

