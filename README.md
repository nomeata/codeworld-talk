CodeWorld talk
==============

This repository contains the program that constitutes my “slides“ for my talk
about lock step simulation, as implemented in [CodeWorld]. This talk was first
held at [Compose Conference 2017], in May 2017, and later at the PLclub at the
University of Pennsylvania, the Haskell Hackers meetup in the Bay Area, ICFP
2017 in Oxford and the Comcast Functional Programming conference 2018 in
Philly.

For more details, see the [extended version of the experience
report](https://arxiv.org/abs/1705.09704) on that topic.

The slides are actually implemented in CodeWorld itself, as one large
`interactionOf`. I wrote the code locally, and in several modules. Using the
hack that is called [hs-all-in-one] I combine and upload the code:

    hs-all-in-one Abstractions.hs Combinators.hs Main.hs Pong.hs \
                  PongState.hs Prediction.hs PredictionLargeStep.hs \
                  PredictionInterpolation.hs TicTacToe.hs \
                  Data/MultiMap.hs Walking.hs \
		  > combined.hs
    ./upload.sh https://code.world/ haskell combined.hs

You can view the resulting presentation at
<https://code.world/run.html?mode=haskell&dhash=DCwU_wEFhmdhjZ-wDI1dtvQ>
and the full code at
<https://code.world/haskell#PzpmgSaSCiJdVrorKAHOv_A>.

Usage:
 * Use `PageUp` and `PageDown` to navigate slides.
 * `P` to unpause paused animations (some start paused, with no visible indication)
 * `Esc` to reset animations
 * The Tic-tac-toe games works by clicking.
 * The Pong game with `W`/`S` resp. `↑`/`↓`.
 * The walking game with `W`/`A`/`D` resp. `↑`/`←`/`→`.
 * The slides where two instances are running in parallel support `O` to show
   them next to each other, and `=` and `-` to modify the delay.

Or simply [play Pong] (this runs [this code]).

[CodeWorld]: https://code.world/
[Compose Conference 2017]: http://www.composeconference.org/2017/program/
[hs-all-in-one]: https://github.com/nomeata/hs-all-in-one
[play Pong]: https://is.gd/codeworldpong
[this code]: https://is.gd/pongcode
