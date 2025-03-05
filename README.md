Can we build an app for open spaces organization?

---

Desirable Characteristics of the stickies

- Common, physical location for people to meet/discuss topics
- No training required for people to write an idea and throw it on the board

---

# Characteristics of the site

- Automatically synced for everyone
- Track interest in a topic
  - Highlight an overbooked time slot
- Automatically find a spot

---

# Don't make a digital version of stickies, unless it serves the UX

---

## Main Menu
- Submit a topic
- View topics
  - Express interest
  - Slot them in
- View schedule

---

#### Technical junk

Websockets client-side
https://laminext.dev/v/0.17.x/websocket

zio-http
https://github.com/zio/zio-http/tree/main/zio-http-example/src/main/scala/example

---

## Submit a topic
- Form
  - Topic

---

## Schedule a topic
- Topic title needs to be visible
- Can we show rooms and time slots simultaneously, while remaining phone-friendly?

---

## Schedule a slot?
- Worth having this perspective, or can we use topic-based scheduling only?

---

## View Topics
- Highlight/filter based on whether or not you have expressed (dis)interest
- Live updates
  - This is an "optimization" that I consider critical to this being useful

---

## View Schedule
- Quick view of the days topics by default
  - This feels like one of the most important views
- Lock days that are too far in the future?
- Show intensity of interest visually
    - Could just be a count
  - Probably not color based, as that can be hard to see for some people

  ### Edit Schedule
  - Not sure if this is a distinct area, or a version of the View Schedule
  - 

## Who are you?
- Name
  - Can we do this in a way where their password is automatically generated and saved in browser?
