# Potential Themes

## First batch

* Use herringbone wang / complete stochastic set tile generation. See [here](https://nothings.org/gamedev/herringbone/index.html).

* Gameplay is about obtaining knowledge.
    * Doing science?
        * Player or player character? Player seems better.
        * A scientist theme is also hard to start with. How about just doing favours for NPCs first?
            * Knowledge can be about how best to navigate the generated space. Maybe don't take on some favours since the trek would be unreasonable?
            * Treasure maps too.
                * Can we make naviagtion with only a static map without a player on it interesting?
                * Take inspiration from Malachite Dreams? See [here](http://www.zincland.com/7drl/explore/).

* Making animations with a state machine currently seems fun. Maybe do more of that?

* Cyclic level generation, like Unexplored? [An article on it](https://www.gamedeveloper.com/disciplines/unexplored-s-secret-cyclic-dungeon-generation-).
    * Maybe use that for puzzle generation, as opposed to actual traversal.
        * That is, allow entering any room, but lock interior doors via these puzzles.

## Second batch

* A fixed size grid is okay, but an infinite grid seems more interesting.
    * Maybe some large fixed amount would be enough. Or we can implement generating
      the same additional tile at a later time.
* If we can solve all the game world's problems, then that world feels dead.
    * So new problems should happen over time.
        * If that happens, then it will feel like the world's can never all be
          solved, becasue they in fact would not be.
            * Okay, so what about other agents that resemble the player which go
              around solving problems? Then maybe any given problem will be solved
              eventually, given enough time. Hopefully that takes the pressure off.
                * Well maybe we want *some* pressure, just not too much. Also, if
                  there are other solvers that seem like they could solve everything
                  then why haven't they solved everything?
                    * An answer here is that the problems require limited resources
                      in order to solve. On an infinite world we can go grab
                      resources from elsewhere to solve local problems. We can then
                      tune the resource amounts to set the pressure amount.
                        * Well wouldn't we be able to tune the solver amounts just
                          as easily?
                            * Fair.
            * Maybe it will be okay anyway. I've seen games before where, you
              couldn't do everything, and I don't think they all felt oppressive.
                * If we go ahead and implement it and then decide stuff feels
                  overly oppressive, it should be possible to either adjust numbers
                  or directly change the rules to make the world's problems solvable.
    * What is the goal of the game then?
        * Just play?
        * Solve the player character's large, complicated problem.
