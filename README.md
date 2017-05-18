CodeWorld talk
==============

This repository contains the program that constitutes my “slides“ for my talk
about lock step simulation, as implemented in [CodeWorld]. This talk was first
held at [Compose Conference 2017], in May 2017.

The slides are actually implemented in CodeWorld itself, as one large
`interactionOf`. I wrote the code locally, and in several modules. Using the
hack that is called [hs-all-in-one] I combine and upload the code:

    hs-all-in-one Abstractions.hs Combinators.hs Main.hs Pong.hs \
                  PongState.hs Prediction.hs PredictionLargeStep.hs \
		  TicTacToe.hs  Data/MultiMap.hs\
		  > combined.hs
    ./upload.sh https://code.world/ haskell combined.hs

You can view the result at
<https://code.world/run.html?mode=haskell&dhash=DkPx1GvE0BVkInzC4pDgGBw>.

Use `PageUp` and `PageDown` to navigate slides, `P` to unpause paused
animations, `Esc` to reset them. The Tic-tac-toe games works by clicking; the
Pong game with `W`/`S` resp. `↑`/`↓`.

Or simply [play Pong].

[CodeWorld]: https://code.world/
[Compose Conference 2017]: http://www.composeconference.org/2017/program/
[hs-all-in-one]: https://github.com/nomeata/hs-all-in-one
[play Pong]: https://is.gd/codeworldpong-