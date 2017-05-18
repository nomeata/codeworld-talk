{-
  Copyright 2016-2017 The CodeWorld Authors. All rights reserved.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}

{- |
This module encapsulates the logics behind the prediction code in the
multi-player setup.
-}

module PredictionLargeStep
    ( Timestamp, StepFun, Future
    , initFuture, currentTimePasses, currentState, addEvent
    )
    where

import qualified Data.IntMap as IM
import qualified Data.MultiMap as M
import Data.Bifunctor (second)
import Data.List (foldl')

type PlayerId = Int
type Timestamp = Double     -- in seconds, relative to some arbitrary starting point

-- All we do with events is to apply them to the state. So let's just store the
-- function that does that.
type Event s = s -> s

-- A state and an event only make sense together with a time.
type TState s = (Timestamp, s)
type TEvent s = (Timestamp, Event s)

type StepFun s = Double -> s -> s
type EventQueue s = M.MultiMap (Timestamp, PlayerId) (Event s)


-- | Invariants about the time stamps in this data type:
-- * committed <= pending <= current < future
-- * The time is advanced with strictly ascending timestamps
-- * For each player, events come in with strictly ascending timestamps
-- * For each player, all events in pending or future are before the
--   corresponding lastEvents entry.
data Future s = Future
        { committed  :: TState s
        , lastQuery  :: Timestamp
        , lastEvents :: IM.IntMap Timestamp
        , pending    :: EventQueue s
        , future     :: EventQueue s
        , current    :: TState s
        }

initFuture :: s -> Int -> Future s
initFuture s numPlayers = Future
    { committed   = (0, s)
    , lastQuery   = 0
    , lastEvents  = IM.fromList [ (n,0) | n <-[0..numPlayers-1]]
    , pending     = M.empty
    , future      = M.empty
    , current     = (0, s)
    }


stepBy :: StepFun s -> Double -> TState s -> TState s
stepBy step diff (now,s)
    = (now + diff, step diff s)

stepTo :: StepFun s -> Timestamp -> TState s -> TState s
stepTo step target (now, s) | target <= now = (now, s)
stepTo step target (now, s) = (target, step (target - now) s)

handleNextEvent :: StepFun s -> TEvent s -> TState s -> TState s
handleNextEvent step (target, event)
    = second event . stepTo step target

handleNextEvents :: StepFun s -> EventQueue s -> TState s -> TState s
handleNextEvents step eq ts
    = foldl' (flip (handleNextEvent step)) ts $
      map (\((t,_p),h) -> (t,h)) $
      M.toList eq

-- | This should be called shortly following 'currentTimePasses'
currentState :: StepFun s -> Timestamp -> Future s -> s
currentState step target f = snd $ stepTo step target (current f)

-- | This should be called regularly, to keep the current state up to date,
-- and to incorporate future events in it.
currentTimePasses :: StepFun s -> Timestamp -> Future s -> Future s
currentTimePasses step target
    = advanceCurrentTime step target
    . advanceFuture step target

-- | Take a new event into account, local or remote.
-- Invariant:
--  * The timestamp of the event is larger than the timestamp
--    of any event added for this player (which is the timestamp for the player
--    in `lastEvents`)
addEvent :: StepFun s ->
    PlayerId -> Timestamp -> Maybe (Event s) ->
    Future s -> Future s
  -- A future event.
addEvent step player now mbEvent f | now > lastQuery f
    = recordActivity step player now $
      f { future     = maybe id (M.insertR (now, player)) mbEvent $ future f }
  -- A past event, goes to pending events. Pending events need to be replayed.
addEvent step player now mbEvent f
    = replayPending step $
      recordActivity step player now $
      f { pending    = maybe id (M.insertR (now, player)) mbEvent $ pending f }

-- | Updates the 'lastEvents' field, and possibly updates the commmitted state
recordActivity :: StepFun s ->
    PlayerId -> Timestamp ->
    Future s -> Future s
recordActivity step player now f
    = advanceCommitted step $
      f { lastEvents = IM.insert player now $ lastEvents f }

-- | Commits events from the pending queue that are past the commitTime
advanceCommitted :: StepFun s -> Future s -> Future s
advanceCommitted step f
    -- | M.null eventsToCommit = f -- do not bother
    | otherwise = f { committed = committed', pending = uncommited' }
  where
    commitTime' = minimum $ IM.elems $ lastEvents f
    canCommit t = t < commitTime'
    (eventsToCommit, uncommited') = M.spanAntitone (canCommit . fst) (pending f)

    committed' = handleNextEvents step eventsToCommit $ committed f

-- | Throws away the current state, and recreates it from
--   pending events. To be used when inserting a pending event.
replayPending :: StepFun s -> Future s -> Future s
replayPending step f = f { current = current' }
  where
    current' =
        stepTo step (lastQuery f) $
        handleNextEvents step (pending f) $
        committed f

-- | Takes into account all future event that happen before the given 'Timestamp'
--   Does not have to call 'replayPending', as we only append to the 'pending' queue.
--   But does have to call 'advanceCommitted', as the newly added events might
--   go directly to the committed state.
advanceFuture :: StepFun s -> Timestamp -> Future s -> Future s
advanceFuture step target f
    | M.null toPerform = f
    | otherwise =
        advanceCommitted step $
        f { current = current', pending = pending', future = future' }
  where
    hasHappened t = t <= target
    (toPerform, future') = M.spanAntitone (hasHappened . fst) (future f)

    pending'  = pending f `M.union` toPerform
    current'  = handleNextEvents step toPerform $ current f

-- | Advances the current time (by big steps)
advanceCurrentTime :: StepFun s -> Timestamp -> Future s -> Future s
advanceCurrentTime step target f
    = f { current = stepTo step target $ current f
        , lastQuery = target }
