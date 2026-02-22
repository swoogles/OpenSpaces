package co.wtf.openspaces

import zio.*

/** Canonical list of GitHub users for random action generation.
  * Used for chaos testing discussions, lightning talks, and hackathon projects.
  */
object RandomUsers:
  val pool: List[Person] = List(
    Person("kitlangton"),
    Person("cheshire137"),
    Person("gaearon"),
    Person("frenck"),
    Person("charliermarsh"),
    Person("peppy"),
    Person("phodal"),
    Person("dtolnay"),
    Person("GrahamCampbell"),
    Person("freekmurze"),
    Person("Borda"),
    Person("antfu"),
    Person("lllyasviel"),
    Person("fabpot"),
    Person("himself65"),
    Person("bradfitz"),
    Person("ornicar"),
  )

  def randomPerson: UIO[Person] =
    Random.nextIntBounded(pool.size).map(pool(_))
