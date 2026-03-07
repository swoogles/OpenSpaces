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
  def circle(latlng: js.Array[Double], options: js.Dynamic): Circle = js.native

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

@js.native
trait Circle extends js.Object:
  def addTo(map: LeafletMap): Circle = js.native
  def remove(): Unit = js.native
  def setLatLng(latlng: js.Array[Double]): Circle = js.native
  def setRadius(radius: Double): Circle = js.native

/** Location map showing all users who are sharing their location */
object LocationMapView:
  // Track map instance, markers, and accuracy circles
  private var mapInstance: Option[LeafletMap] = None
  private var markers: Map[Person, Marker] = Map.empty
  private var accuracyCircles: Map[Person, Circle] = Map.empty
  // Track pending map initialization
  private var pendingMapId: Option[String] = None
  private var resizeObserver: Option[js.Dynamic] = None

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
              // Initialize map on mount with proper event-driven approach
              onMountCallback { ctx =>
                initializeMapWhenReady(mapId, ctx.thisNode.ref)
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

  /** Initialize map when the element has actual dimensions */
  private def initializeMapWhenReady(mapId: String, element: dom.Element): Unit =
    // Check if Leaflet is loaded
    if js.isUndefined(js.Dynamic.global.L) then
      dom.console.warn("Leaflet not loaded yet")
      return

    pendingMapId = Some(mapId)

    // Use ResizeObserver for proper event-driven initialization
    // This fires when the element gets actual dimensions
    if !js.isUndefined(js.Dynamic.global.ResizeObserver) then
      val observer = js.Dynamic.newInstance(js.Dynamic.global.ResizeObserver)(
        (entries: js.Array[js.Dynamic]) => {
          entries.foreach { entry =>
            val rect = entry.contentRect
            val width = rect.width.asInstanceOf[Double]
            val height = rect.height.asInstanceOf[Double]
            
            // Only initialize once we have actual dimensions
            if width > 0 && height > 0 && pendingMapId.contains(mapId) && mapInstance.isEmpty then
              pendingMapId = None
              initializeMap(mapId)
          }
        }
      )
      observer.observe(element)
      resizeObserver = Some(observer)
    else
      // Fallback for browsers without ResizeObserver (rare)
      // Use requestAnimationFrame loop instead of setTimeout
      def checkDimensions(): Unit =
        val rect = element.getBoundingClientRect()
        if rect.width > 0 && rect.height > 0 && pendingMapId.contains(mapId) && mapInstance.isEmpty then
          pendingMapId = None
          initializeMap(mapId)
        else if pendingMapId.contains(mapId) then
          dom.window.requestAnimationFrame(_ => checkDimensions())
      
      dom.window.requestAnimationFrame(_ => checkDimensions())

  private def initializeMap(mapId: String): Unit =
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

  /** Format accuracy for display */
  private def formatAccuracy(accuracy: Double): String =
    if accuracy <= 0 then ""
    else if accuracy < 10 then s"±${accuracy.toInt}m"
    else if accuracy < 100 then s"±${(accuracy / 5).round * 5}m"  // Round to nearest 5m
    else if accuracy < 1000 then s"±${(accuracy / 10).round * 10}m"  // Round to nearest 10m
    else s"±${(accuracy / 100).round * 100}m"  // Round to nearest 100m

  private def updateMarkers(locations: List[SharedLocation]): Unit =
    mapInstance.foreach { map =>
      val currentPersons = locations.map(_.person).toSet
      
      // Remove markers and circles for users no longer sharing
      markers.foreach { case (person, marker) =>
        if !currentPersons.contains(person) then
          marker.remove()
      }
      accuracyCircles.foreach { case (person, circle) =>
        if !currentPersons.contains(person) then
          circle.remove()
      }
      markers = markers.filter { case (person, _) => currentPersons.contains(person) }
      accuracyCircles = accuracyCircles.filter { case (person, _) => currentPersons.contains(person) }

      // Add/update markers and circles for current sharers
      locations.foreach { location =>
        val latlng = js.Array(location.latitude, location.longitude)
        val displayName = location.displayName.getOrElse(location.person.unwrap)
        val ageMinutes = (System.currentTimeMillis() - location.timestamp) / 60000
        val ageText = if ageMinutes < 1 then "just now" 
                      else if ageMinutes < 60 then s"${ageMinutes}m ago"
                      else s"${ageMinutes / 60}h ago"
        
        // Format accuracy for popup
        val accuracyText = formatAccuracy(location.accuracy)
        val accuracyHtml = if accuracyText.nonEmpty then
          s"""<br/><span class="accuracy" style="color: #666; font-size: 0.9em;">Accuracy: $accuracyText</span>"""
        else ""

        val popupContent = s"""
          <div class="LocationPopup">
            <strong>$displayName</strong>$accuracyHtml<br/>
            <small>Updated $ageText</small><br/>
            <a href="https://www.google.com/maps/dir/?api=1&destination=${location.latitude},${location.longitude}" 
               target="_blank" rel="noopener">
              Get Directions
            </a>
          </div>
        """.trim

        // Handle marker
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

        // Handle accuracy circle
        val acc = location.accuracy
        if acc > 0 then
          accuracyCircles.get(location.person) match
            case Some(existingCircle) =>
              // Update existing circle
              existingCircle.setLatLng(latlng)
              existingCircle.setRadius(acc)
            case None =>
              // Create new accuracy circle
              val circleOptions = js.Dynamic.literal(
                radius = acc,
                color = "#3388ff",
                fillColor = "#3388ff",
                fillOpacity = 0.15,
                weight = 1,
                opacity = 0.5
              )
              val circle = Leaflet.circle(latlng, circleOptions)
              circle.addTo(map)
              accuracyCircles = accuracyCircles + (location.person -> circle)
        else
          // Remove circle if accuracy not available
          accuracyCircles.get(location.person).foreach(_.remove())
          accuracyCircles = accuracyCircles - location.person
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
    // Disconnect resize observer
    resizeObserver.foreach(_.disconnect())
    resizeObserver = None
    pendingMapId = None
    
    // Clean up circles
    accuracyCircles.values.foreach(_.remove())
    accuracyCircles = Map.empty
    
    // Clean up markers
    markers.values.foreach(_.remove())
    markers = Map.empty
    mapInstance = None
  
  extension [A](seq: Seq[A])
    def toJSArray: js.Array[A] = js.Array(seq*)
