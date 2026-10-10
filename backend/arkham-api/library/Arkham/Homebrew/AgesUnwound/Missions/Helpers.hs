{- | The @missions@ encounter set's shared /Task/ convention.

Scenario V's guide (@docs/homebrew/data/au-guide-pp01-20.md@ p12):

> Some treacheries in this scenario have the __Task__ trait. These cards have
> objectives on them, which provide goals to complete throughout the scenario.
> When you meet the objective printed on a Task, or when another card tells you
> to complete it, remove it from the game, making sure to keep track of which
> Tasks you've completed.

That printed rule /is/ the implementation: completing a Task removes its card
from the game, and "which Tasks you've completed" is read back out of the
removed-from-game pile with 'getCompletedTasks'. No campaign-log key and no
scenario meta are involved, so nothing here collides with the scenario module.

The convention holds only while this invariant does: __a Task treachery leaves
play exactly once, by being completed.__ No card in @missions@ or
@a_year_to_plan@ discards or removes an uncompleted Task, so every @Task@-trait
card in 'getRemovedFromPlayCards' is a completed one. Anything new that removes
an uncompleted Task has to route around 'completeTask'.
-}
module Arkham.Homebrew.AgesUnwound.Missions.Helpers (
  module Arkham.Homebrew.AgesUnwound.Missions.Helpers,
) where

import Arkham.Card
import Arkham.Card.PlayerCard (setPlayerCardOwner)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Helpers.FetchCard (FetchCard, fetchCard)
import Arkham.Helpers.Game (getRemovedFromPlayCards)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Id
import Arkham.Matcher
import Arkham.Message (Message (RemovedFromGame, ResolveTreachery))
import Arkham.Message.Lifted (
  addToHand,
  createTreacheryAt_,
  gameModifier,
  takeControlOfSetAsideAsset,
 )
import Arkham.Message.Lifted.Card (removeFromGame)
import Arkham.Message.Lifted.Placement
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Modifier (ModifierType (DoNotTakeUpSlot))
import Arkham.Prelude
import Arkham.Projection
import Arkham.SlotType (SlotType)
import Arkham.Source (Sourceable)
import Arkham.Target (Target (CardIdTarget))
import Arkham.Trait (Trait (Task))
import Arkham.Treachery.Types (Field (TreacheryCard), TreacheryAttrs)

{- | The six Tasks shuffled into the @TaskDeck@ at setup.

Scenario V's setup list, in the guide's order. /The Long Game/ puts the top card
of this deck into play next to the act deck, resolving its revelation.
-}
taskDeckCards :: [CardDef]
taskDeckCards =
  [ Treacheries.enemyOfMyEnemy
  , Treacheries.aTreasureUnearthed
  , Treacheries.theDevilYouKnow
  , Treacheries.higherPowers
  , Treacheries.entreatingTheGods
  , Treacheries.upToSomething
  ]

{- | Every Task in the scenario: the deck's six plus the five that enter play on
their own schedule -- /Helping Yourself/ at setup, /Keeper of Knowledge/ and
/Strange Portal/ off Autumn's back, /The Tunguska Event/ off Winter's, and
/Final Preparations/ off Spring's.
-}
allTasks :: [CardDef]
allTasks =
  [ Treacheries.helpingYourself
  , Treacheries.keeperOfKnowledge
  , Treacheries.strangePortal
  , Treacheries.theTunguskaEvent
  , Treacheries.finalPreparations
  ]
    <> taskDeckCards

{- | Where a Task sits once it is in play: "next to the act deck", per both the
setup instruction and /The Long Game/'s ability. Centralised so every Task
places itself the same way and whoever resolves the revelation does not have to.

A Task with a printed __Revelation__ calls this as the first thing it does, so
@After (Revelation ...)@ no longer sees 'Limbo' and does not discard it.
-}
placeThisTask :: ReverseQueue m => TreacheryAttrs -> m ()
placeThisTask attrs = place attrs NextToAct

{- | "Put the <Task> treachery into play next to the act deck" -- /Helping
Yourself/ at setup, /The Tunguska Event/ off Winter, /Final Preparations/ off
Spring. None of those three print a __Revelation__, so nothing is resolved.
-}
putTaskIntoPlay :: ReverseQueue m => CardDef -> m ()
putTaskIntoPlay def = createTreacheryAt_ def NextToAct

{- | "...into play next to the act deck, resolving its revelation effect" --
/The Long Game/'s ability and Autumn's back.

Created in 'Limbo', which is the placement a freshly drawn treachery has, then
handed to 'ResolveTreachery': that is the engine's "resolve this treachery as if
just drawn" entry point, so the revelation gets its @#when@/@#after@ frame and
honours 'Arkham.Modifier.IgnoreRevelation'. The Task's own handler moves it to
'NextToAct' via 'placeThisTask'.
-}
putTaskIntoPlayWithRevelation :: (ReverseQueue m, FetchCard card) => InvestigatorId -> card -> m ()
putTaskIntoPlayWithRevelation iid c = do
  card <- fetchCard c
  (tid, msg) <- Msg.createTreacheryAt card Limbo
  push msg
  push $ ResolveTreachery iid tid

-- | Complete the named Task, if it is in play.
completeTask :: ReverseQueue m => CardDef -> m ()
completeTask def = selectOne (treacheryIs def) >>= traverse_ completeTaskId

{- | Complete a Task by id -- what a Task completing /itself/ calls, passing its
own attrs.
-}
completeTaskId :: (ReverseQueue m, AsId a, IdOf a ~ TreacheryId) => a -> m ()
completeTaskId (asId -> tid) = do
  card <- field TreacheryCard tid
  push $ RemovedFromGame card
  removeFromGame tid

-- | The Tasks completed so far, as cards.
getCompletedTasks :: HasGame m => m [Card]
getCompletedTasks = filterCards (CardWithTrait Task) <$> getRemovedFromPlayCards

{- | "X is the number of completed [[Tasks]]" -- Scenario V's [skull] token, and
the count its Resolution 1 branches on.
-}
getCompletedTaskCount :: HasGame m => m Int
getCompletedTaskCount = length <$> getCompletedTasks

-- | Whether the named Task has been completed.
taskCompleted :: HasGame m => CardDef -> m Bool
taskCompleted def = any ((== toCardCode def) . toCardCode) <$> getCompletedTasks

{- | "Put the set-aside <asset> into play under your control. For the remainder
of this scenario, it does not take up a <slot> slot."

Four of the six player-card rewards say exactly this (Chronal Atlas/hand, Ionian
Pendant/accessory, Wings of Damakairon/body, Forestall Fate/arcane). The slot
suppression is applied to the card rather than to the asset, because the asset
does not exist yet when @TakeControlOfSetAsideAsset@ is still in the queue --
@AssetSlots@ reads the card's modifiers alongside the asset's for exactly this
case.
-}
takeControlOfSetAsideRewardAsset
  :: (ReverseQueue m, Sourceable source)
  => source -> InvestigatorId -> CardDef -> SlotType -> m ()
takeControlOfSetAsideRewardAsset source iid def slot = do
  card <- getSetAsideCard def
  takeControlOfSetAsideAsset iid card
  gameModifier source (CardIdTarget $ toCardId card) (DoNotTakeUpSlot slot)

{- | "Add the set-aside <card> to your hand. For the remainder of this scenario,
you are considered to own this card."

/Gratitude/ (Monastic Training) and /Grudging Assistance/ (Agency Strike Team).
Ownership is the card's @pcOwner@, so the set-aside copy is reissued with the
owner set before it reaches the hand -- the same move
@AddCampaignCardToDeck@ makes.
-}
addSetAsideCardToHandAsOwner :: ReverseQueue m => InvestigatorId -> CardDef -> m ()
addSetAsideCardToHandAsOwner iid def = do
  card <- getSetAsideCard def
  let card' = overPlayerCard (setPlayerCardOwner iid) card
  replaceCard (toCardId card) card'
  addToHand iid [card']
