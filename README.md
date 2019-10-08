# MinePlace.social

“In that famous mining game, you build a mine. In Mineplace, you live in a mansion.”

It’s pronounced "MINE-place", with the first word being a pun on the personal possessive pronoun and a tunnel where valuable commodities are dug from the ground, and the whole being a curtsy in the direction of the fabulously successful networked Java game.

## History

Way back in 2012, when I first heard about [Node.JS](https://nodejs.org/), I recreated a maze game that I played on the PDP-10 and Imlac terminals at MIT in the late 1970s. In that game, a bunch of players ran around a shared maze, shooting at each other.

That game was up at [jsmaze.com](jsmaze.com) for many years, but I took it down to save the $15/month that its AWS VM was costing me. I spent a little time attempting to make it work again, but the libraries it depends on had changed incompatibily, and having been spoiled by Elm, I didn't want to spend the effort it would take to figure it out.

This project started as a remake, from scratch, in Elm, and has morphed into a federated social media interface for the spatially-oriented.

## Tech

See [persistence.md](https://github.com/billstclair/elm-jsmaze/blob/master/Persistence.md) for information about how boards and players are persisted.

To run the code:

    cd .../mineplace
    bin/build
    elm reactor
    
(where ".../mineplace" denotes the path on your machine to where you `git clone`d the code. Then aim your browser at [http://localhost:8000/site/index.html](localhost:8000/site/index.html).

Bill St. Clair, 7 October 2019
