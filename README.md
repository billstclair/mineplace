# [MinePlace.social](https://mineplace.social/)

“In that famous mining game, you build a mine. In MinePlace, you live in a mansion.”

It’s pronounced "mine-PLACE", with the first word being a pun on the personal possessive pronoun and a tunnel where valuable commodities are dug from the ground, and the whole being a curtsy in the direction of the fabulously successful networked Java game. You may also think of it in German as "Mein Platz" or French "Ma Place".

MinePlace allows you to interact with others in a physical space that you acquire or build. You can use it with scripting to create games, collect and display images, video, and sculpture, talk or type with visitors, and federate with other places.

My alpha graphics is simple line drawings, but it will use WebGL very soon for realistic three-dimensional rendering.

One hour a day of server connection will always be available for free, at least from my server, but I will charge $1 per month for unlimited server use, with a storage cap that you can exceed with your own S3 account, at Amazon, Digital Ocean, or any S3-compatible server.

## History

Way back in 2012, when I first heard about [Node.JS](https://nodejs.org/), I recreated a maze game that I played on the PDP-10 and Imlac terminals at MIT in the late 1970s. In that game, a bunch of players ran around a shared maze, shooting at each other.

That game was up at [jsmaze.com](jsmaze.com) for many years, but I took it down to save the $15/month that its AWS VM was costing me. I spent a little time attempting to make it work again, but the libraries it depends on had changed incompatibily, and having been spoiled by Elm, I didn't want to spend the effort it would take to figure it out.

This project started as a remake, from scratch, in Elm, and has morphed into a federated social media interface for the spatially-oriented.

## Tech

The client-side code is Elm compiled to JavaScript, which will run in any modern web browser. I'll wrap $0.99 iOS and Android apps around the webapp, but you can always use a mobile browser for free.

The server side is initially Elm on Node.js, using [billstclair/elm-websocket-framework package](http://package.elm-lang.org/packages/billstclair/elm-websocket-framework/latest), but if that doesn't scale well enough, I'll switch to [elm-beam](https://github.com/hkgumbs/elm-beam), if it's up to the task by then.

See [persistence.md](https://github.com/billstclair/elm-jsmaze/blob/master/Persistence.md) for information about how boards and players are persisted.

To run the code:

    cd .../mineplace
    bin/build
    elm reactor
    
(where ".../mineplace" denotes the path on your machine to where you `git clone`d the code. Then aim your browser at [http://localhost:8000/site/index.html](localhost:8000/site/index.html).

Bill St. Clair, 7 October 2019
