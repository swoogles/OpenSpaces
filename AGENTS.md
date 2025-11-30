Anytime you are working in the frontend, remember that you are using strongly-typed ScalaJS.
Using dynamic JS stuff should be an absolute last resort.
Laminar is your ScalaJS UI framework of choice - stick to it.

This application is intended exclusively to be used on mobile devices.
Every UI element/behavior should look nice and obvious on a small screen.

The model that is being updated by websocket actions should ensure that everyone gets an accurate representation.
