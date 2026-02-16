package co.wtf.openspaces

import org.scalajs.dom
import org.scalajs.dom.ServiceWorkerRegistrationOptions
import org.scalajs.dom.experimental.serviceworkers.toServiceWorkerNavigator

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.{Failure, Success}

object ServiceWorkerClient:
  private var reloadedForNewServiceWorker = false

  def registerServiceWorker(): Unit =
    val window = dom.window
    val navigatorDyn = window.navigator.asInstanceOf[js.Dynamic]
    if js.isUndefined(navigatorDyn.selectDynamic("serviceWorker")) then
      return

    val serviceWorker =
      toServiceWorkerNavigator(window.navigator).serviceWorker

    serviceWorker
      .register(
        "/sw.js",
        new ServiceWorkerRegistrationOptions {
          scope = "/"
        },
      )
      .toFuture
      .onComplete {
        case Success(registration) =>
          registration.onupdatefound = (_: dom.Event) => ()
          if registration.waiting != null then
            registration.waiting.postMessage("SKIP_WAITING")
          registration.update()

          val controller = serviceWorker.controller
          if controller != null then
            println("SWC: controller present -> " + controller.state)
          serviceWorker.oncontrollerchange = (_: dom.Event) =>
            println("SWC: controller changed")
            if !reloadedForNewServiceWorker then
              reloadedForNewServiceWorker = true
              dom.window.location.reload()

        case Failure(error) =>
          println(s"SWC: registration failed: ${error.getMessage}")
      }
