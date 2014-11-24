## slick-crud - Admin tool for Slick database tables

slick-crud exposes your database tables through a web editor interface, so you can free up your
time from writing mundane admin tools.

It provides the following features:
- Typeclass driven rendering and parsing of values
- Browsing data (table editors are composable and are linked, and can be presented according to a database query (sorting, filtering, projections, etc))
- Interface for creating new rows and updating individual values, both in a typesafe way
- Deleting rows
- Can mark types and/or tables as read-only
- Optional/Nullable values
- Notifications of (failed) updates

slick-crud is available for slick 2.1.0 on scala 2.11.
 
Initial idea and implementation by [@teigen][teigen], maintenance/rewrite by [@elacin][elacin]

## Usage

slick-crud requires tables with a single (non-composite) primary key. It also requires instances of `SimpleCell`
 for every type which is used in a `Table`. All the individual cells are automatically collected in a tupled `CellRow`.
 If your table is projected to a type other than a Tuple, you also need to use `mappedCellRow()`
 to provide mapping back and forth to a tuple representation.

A complete, runnable example is available [here][demo].

Run it by cloning the git project and run `sbt run`


## Implementation

The bundled [concretization][crud-unfiltered] of slick-crud uses [unfiltered][unfiltered]
for http, and Html5 with to render the frontend.
The core of slick-crud is kept abstract, without much knowledge of neither, hence it should be relatively
easy to replace any if they don't fit your preferences.

## Status
Slick doesn't exactly enable this use out of the box, which resulted in some [hairy][columnPicker] [code][queryParser].
Although we have used this for a long while internally, we fully expect there to be issues as people use
it more creatively. Bug reports welcome :)

We will try to keep the public API relatively constant, but no guarantees just yet.

[teigen]: https://github.com/teigen
[elacin]: https://github.com/elacin
[demo]: demo/src/main/scala/no/penger/crud/CrudDemoWebApp.scala
[crud-unfiltered]: unfiltered/src/main/scala/no/penger/crud
[unfiltered]: https://github.com/unfiltered/unfiltered
[columnPicker]: core/src/main/scala/no/penger/crud/columnPicker.scala
[queryParser]: core/src/main/scala/no/penger/crud/queryParser.scala