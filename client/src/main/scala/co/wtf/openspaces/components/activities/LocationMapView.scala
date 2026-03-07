package co.wtf.openspaces.components.activities

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import neotype.unwrap

import co.wtf.openspaces.{AppState, Person, GitHubAvatar}
import co.wtf.openspaces.location.SharedLocation
import co.wtf.openspaces.services.GeolocationService

/** Leaflet map bindings for Scala.js */
@js.native
@JSGlobal("L")
object Leaflet extends js.Object:
  def map(id: String): LeafletMap = js.native
  def tileLayer(url: String, options: js.Dynamic): TileLayer = js.native
  def marker(latlng: js.Array[Double], options: js.Dynamic): Marker = js.native
  def popup(): Popup = js.native
  def icon(options: js.Dynamic): LeafletIcon = js.native

@js.native
trait LeafletMap extends js.Object:
  def setView(latlng: js.Array[Double], zoom: Int): LeafletMap = js.native
  def addLayer(layer: js.Any): LeafletMap = js.native
  def removeLayer(layer: js.Any): LeafletMap = js.native
  def invalidateSize(): Unit = js.native
  def fitBounds(bounds: js.Array[js.Array[Double]], options: js.Dynamic): LeafletMap = js.native

@js.native
trait TileLayer extends js.Object:
  def addTo(map: LeafletMap): TileLayer = js.native

@js.native
trait Marker extends js.Object:
  def addTo(map: LeafletMap): Marker = js.native
  def remove(): Unit = js.native
  def bindPopup(content: String): Marker = js.native
  def setLatLng(latlng: js.Array[Double]): Marker = js.native
  def getLatLng(): js.Dynamic = js.native

@js.native
trait Popup extends js.Object:
  def setContent(content: String): Popup = js.native

@js.native
trait LeafletIcon extends js.Object

/** Location map showing all users who are sharing their location */
object LocationMapView:
  // Track map instance and markers
  private var mapInstance: Option[LeafletMap] = None
  private var markers: Map[Person, Marker] = Map.empty

  def apply(): HtmlElement =
    val expanded = Var(false)
    val mapId = s"location-map-${scala.util.Random.nextInt(10000)}"

    div(
      cls := "LocationMap",
      // Header with toggle
      div(
        cls := "LocationMap-header",
        h3(cls := "LocationMap-title", "📍 Find Your People"),
        span(
          cls := "LocationMap-count",
          child.text <-- AppState.sharersCount.map(count =>
            if count == 0 then "No one sharing"
            else if count == 1 then "1 person sharing"
            else s"$count people sharing"
          )
        ),
        button(
          cls := "LocationMap-toggle",
          child.text <-- expanded.signal.map(if _ then "Hide Map" else "Show Map"),
          onClick --> Observer(_ => expanded.update(!_)),
        ),
      ),
      // Map container (shown when expanded)
      child <-- expanded.signal.combineWith(AppState.sharersCount).map {
        case (true, count) if count > 0 =>
          div(
            cls := "LocationMap-container",
            div(
              idAttr := mapId,
              cls := "LocationMap-leaflet",
              // Initialize map on mount
              onMountCallback { ctx =>
                // Small delay to let DOM settle
                dom.window.setTimeout(() => initializeMap(mapId), 100)
              },
              onUnmountCallback { _ =>
                cleanupMap()
              },
            ),
            // Update markers when locations change
            AppState.locationState.signal --> Observer { (state: co.wtf.openspaces.location.LocationState) =>
              updateMarkers(state.locations.values.toList)
            },
          )
        case (true, 0) =>
          div(
            cls := "LocationMap-empty",
            p("No one is sharing their location right now."),
            p("Toggle the ", span(cls := "emoji", "📌"), " button in the top bar to share yours!"),
          )
        case _ =>
          emptyNode
      },
    )

  private def initializeMap(mapId: String): Unit =
    // Check if Leaflet is loaded
    if js.isUndefined(js.Dynamic.global.L) then
      dom.console.warn("Leaflet not loaded yet")
      return

    val mapElement = dom.document.getElementById(mapId)
    if mapElement == null then
      dom.console.warn(s"Map element $mapId not found")
      return

    try
      val map = Leaflet.map(mapId)
      
      // Default center (Denver - WTF location area)
      map.setView(js.Array(39.7392, -104.9903), 13)

      // Add OpenStreetMap tiles
      val tiles = Leaflet.tileLayer(
        "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        js.Dynamic.literal(
          attribution = "&copy; OpenStreetMap contributors",
          maxZoom = 19
        )
      )
      tiles.addTo(map)

      mapInstance = Some(map)
      
      // Add initial markers
      val locations = AppState.locationState.now().locations.values.toList
      updateMarkers(locations)
      
    catch
      case e: Throwable =>
        dom.console.error(s"Error initializing map: ${e.getMessage}")

  private def updateMarkers(locations: List[SharedLocation]): Unit =
    mapInstance.foreach { map =>
      val currentPersons = locations.map(_.person).toSet
      
      // Remove markers for users no longer sharing
      markers.foreach { case (person, marker) =>
        if !currentPersons.contains(person) then
          marker.remove()
      }
      markers = markers.filter { case (person, _) => currentPersons.contains(person) }

      // Add/update markers for current sharers
      locations.foreach { location =>
        val latlng = js.Array(location.latitude, location.longitude)
        val displayName = location.displayName.getOrElse(location.person.unwrap)
        val ageMinutes = (System.currentTimeMillis() - location.timestamp) / 60000
        val ageText = if ageMinutes < 1 then "just now" 
                      else if ageMinutes < 60 then s"${ageMinutes}m ago"
                      else s"${ageMinutes / 60}h ago"

        val popupContent = s"""
          <div class="LocationPopup">
            <strong>$displayName</strong><br/>
            <small>Updated $ageText</small><br/>
            <a href="https://www.google.com/maps/dir/?api=1&destination=${location.latitude},${location.longitude}" 
               target="_blank" rel="noopener">
              Get Directions
            </a>
          </div>
        """.trim

        markers.get(location.person) match
          case Some(existingMarker) =>
            // Update position
            existingMarker.setLatLng(latlng)
            existingMarker.bindPopup(popupContent)
          case None =>
            // Create new marker with custom icon
            val markerOptions = js.Dynamic.literal(
              title = displayName
            )
            val marker = Leaflet.marker(latlng, markerOptions)
            marker.bindPopup(popupContent)
            marker.addTo(map)
            markers = markers + (location.person -> marker)
      }

      // Fit bounds if we have locations
      if locations.nonEmpty && locations.size > 1 then
        val bounds = locations.map(l => js.Array(l.latitude, l.longitude)).toJSArray
        map.fitBounds(bounds, js.Dynamic.literal(padding = js.Array(50, 50)))
      else if locations.size == 1 then
        val loc = locations.head
        map.setView(js.Array(loc.latitude, loc.longitude), 15)
    }

  private def cleanupMap(): Unit =
    markers.values.foreach(_.remove())
    markers = Map.empty
    mapInstance = None
  
  extension [A](seq: Seq[A])
    def toJSArray: js.Array[A] = js.Array(seq*)
