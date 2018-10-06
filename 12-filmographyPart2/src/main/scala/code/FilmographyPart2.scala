package code

// Exercise:
//
// Complete the methods below
//
// The source code for Director and Film,
// and the FilmTestData used in the unit tests,
// are in the "utilities" project.

object FilmographyPart2 extends Exercise {

  def directorsWithBackCatalogOfSize(directors: List[Director], numberOfFilms: Int): List[Director] =
    directors.filter { d => d.films.length >= numberOfFilms }

  def directorsBornBefore(directors: List[Director], year: Int): List[Director] =
    directors filter { d => d.yearOfBirth < year }

  def directorsBornBeforeWithBackCatalogOfSize(directors: List[Director], year: Int, numberOfFilms: Int): List[Director] =
    for {
      director <- directors
      if director.yearOfBirth < year
      if director.films.length == numberOfFilms
    } yield director

  def findDirectorWithName(directors: List[Director], lastName: String): Option[Director] =
    directors find {
      _.lastName.toLowerCase == lastName.toLowerCase
    }

  def findDirectorWithBackCatalogOfSize(directors: List[Director], numberOfFilms: Int): Option[Director] =
    directors find {
      _.films.length >= numberOfFilms
    }

  def findDirectorBornBefore(directors: List[Director], year: Int): Option[Director] =
    directors find {
      _.yearOfBirth < year
    }

  def findDirectorBornBeforeWithBackCatalogOfSize(directors: List[Director], year: Int, numberOfFilms: Int): Option[Director] =
    findDirectorWithBackCatalogOfSize(directorsBornBefore(directors, year), numberOfFilms)

  def allFilms(directors: List[Director]): List[Film] =
    directors flatMap {
      _.films
    }

  def namesOfAllFilms(directors: List[Director]): List[String] =
    allFilms(directors) map {
      _.name
    }

  def allFilmsWithRatingOver(directors: List[Director], imdbRating: Double): List[Film] =
    directors.flatMap{_.films}.filter(_.imdbRating > imdbRating)

  def allFilmsByDirector(directors: List[Director], lastName: String): List[Film] =
    directors filter {
      _.lastName.toLowerCase == lastName.toLowerCase
    } flatMap {
      _.films
    }

  def namesOfAllFilmsByDirector(directors: List[Director], lastName: String): List[String] =
    allFilmsByDirector(directors, lastName) map {
      _.name
    }

  def totalImdbRating(films: List[Film]): Double =
    films.foldLeft(0.0) { (acc, film) => acc + film.imdbRating }

  def averageImdbRating(films: List[Film]): Double =
    if (films.isEmpty) 0.0
    else totalImdbRating(films) / films.length

  def directorsSortedByAge(directors: List[Director], ascending: Boolean = true): List[Director] =
    directors.sorted((x: Director, y: Director) =>
      if (ascending) x.yearOfBirth - y.yearOfBirth
      else y.yearOfBirth - x.yearOfBirth
    )

  case class DirectorWithFilms(director: Director, films: List[Film])

  def directorWithHighestAverageImdbRating(directors: List[Director]): Option[Director] =
    if (directors.isEmpty) None
    else Some(directors.maxBy(d => averageImdbRating(d.films)))

  def earliestFilmByAnyDirector(directors: List[Director]): Option[Film] =
    if (directors.isEmpty) None
    else {
      val allDirectorsWithEarliestMovie = directors.filter(d => d.films.nonEmpty).map(d => d.films.minBy(_.yearOfRelease))

      Some(allDirectorsWithEarliestMovie.minBy(d => d.yearOfRelease))
    }

  def earliestFilmsByAllDirectors(directors: List[Director]): Map[Director, Option[Film]] = {
    def earliestFilm(films: List[Film]): Option[Film] = {
      if(films.isEmpty) None
      else Some(films.minBy(_.yearOfRelease))
    }

    directors.map(d => DirectorWithFilms(d, d.films))
      .map(r => (r.director, earliestFilm(r.films)))
      .toMap
  }

}
