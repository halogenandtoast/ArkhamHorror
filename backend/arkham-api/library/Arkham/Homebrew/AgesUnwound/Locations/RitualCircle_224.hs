{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Homebrew.AgesUnwound.Locations.RitualCircle_224 (ritualCircle_224) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection

newtype RitualCircle_224 = RitualCircle_224 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /A Harnessed Future/. Set aside; Scenario III puts it into play.
ritualCircle_224 :: LocationCard RitualCircle_224
ritualCircle_224 = symbolLabel $ location RitualCircle_224 Cards.ritualCircle_224 4 (PerPlayer 1)

{- | An investigator this location has taken out of the game, and the actions
they still had when it happened.

The count is what makes the return a /resumed/ turn rather than a fresh one:
@Do BeginRound@ re-derives @remainingActions@ for everyone before the
investigation phase, so the leftover has to be carried across and put back
afterwards. See 'resumeActions'.
-}
data Suspension = Suspension
  { investigator :: InvestigatorId
  , actionsRemaining :: Maybe Int
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

{- | A seat suspended before the count was recorded was stored as a bare id.
Such a seat still comes back -- with an ordinary turn, since there is nothing
to resume it to -- rather than being dropped by a decode failure and stranded
out of the game forever.
-}
instance FromJSON Suspension where
  parseJSON v =
    (genericParseJSON defaultOptions v)
      <|> (flip Suspension Nothing <$> parseJSON v)

{- | The investigators this location has taken out of the game, pending their
return at the start of the next investigation phase.
-}
suspended :: LocationAttrs -> [Suspension]
suspended = getLocationMetaDefault []

{- | While an investigator is out of the game, nothing may reach them. Four core
loops iterate investigators without checking placement, so the ban is spelled
out as modifiers (the approach recorded in
@docs/homebrew/data/ages-unwound-engine-notes.md@ for out-of-play seats).

The second printed line -- "cannot willingly end their turn while they have
actions remaining" -- is the same shape, so it rides here too: 'CannotEndTurn'
only takes the end-turn button off the player window, and the window is rebuilt
with fresh modifiers after every action, so the button comes back the moment
the last action is spent. @InvestigatorWithActionsRemaining@ and not the
@Standard@ variant because the card says "actions remaining", where
/Classroom/ says "standard action".
-}
instance HasModifiersFor RitualCircle_224 where
  getModifiersFor (RitualCircle_224 a) = do
    for_ (suspended a) \s ->
      modified_
        a
        s.investigator
        [ CannotDrawCards
        , CannotGainResources
        , CannotTakeAction IsAnyAction
        , CannotPlay AnyCard
        , CannotMove
        , CannotBeAttacked
        , CannotBeEngaged
        ]
    modifySelect
      a
      (InvestigatorAt (be a) <> InvestigatorWithActionsRemaining (atLeast 1))
      [CannotEndTurn]

{- | "Haunted - Suspend your current turn and remove your investigator from the
game. At the start of the next investigator phase, return your investigator to
this location and resume the current turn. /
Investigators at this location cannot willingly end their turn while they have
actions remaining."

Ability 2 is the engine hook that brings them back, so it is silent -- the card
prints no second ability.

The last line is a 'CannotEndTurn' modifier; see 'HasModifiersFor' above.

/Resume the current turn/ is the leftover action count travelling with the
seat. It cannot be restored at suspension time -- @Do BeginRound@ re-derives
@remainingActions@ for the new round before the investigation phase is reached,
so anything put back here is overwritten -- and it is not restored at turn
start either, since @BeginTurn@ never touches the count. Ability 2 is the one
moment that works: the phase has begun, the round's derivation is done, and the
turn has not started. See 'resumeActions'.

What the engine still cannot do is make it literally the same turn: the
investigator takes a turn in the next investigation phase carrying the actions
they had left, but @actionsTaken@, @actionsPerformed@ and every per-turn
ability limit reset the way they would for any turn, and @TurnBegins@ is raised
again. Holding those across a phase boundary needs real turn suspension, which
no card but this one has ever asked for.
-}
instance HasAbilities RitualCircle_224 where
  getAbilities (RitualCircle_224 a) =
    extendRevealed a
      $ campaignI18n (hauntedI "ritualCircleAHarnessedFuture.haunted" a 1)
      : [ mkAbility a 2 $ SilentForcedAbility $ PhaseBegins #when #investigation
        | notNull (suspended a)
        ]

{- | Put back the actions the seat was suspended with.

Safe here and nowhere earlier: @Do BeginRound@ has already given everyone their
actions for the new round and @BeginTurn@ will not touch the count again, so a
'SetActions' at the start of the investigation phase is what the investigator
takes their turn with. A seat recorded before the count existed has nothing to
put back and takes an ordinary turn.
-}
resumeActions :: (ReverseQueue m, Sourceable source) => source -> Suspension -> m ()
resumeActions source s = for_ s.actionsRemaining (setActions s.investigator source)

instance RunMessage RitualCircle_224 where
  runMessage msg (RitualCircle_224 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Read before the turn is taken away, or there is nothing left to resume.
      remaining <- field InvestigatorRemainingActions iid
      setActions iid (attrs.ability 1) 0
      endYourTurn iid
      push $ PlaceInvestigator iid Unplaced
      let kept = filter ((/= iid) . (.investigator)) (suspended attrs)
      pure . RitualCircle_224 $ attrs & setMeta (Suspension iid (Just remaining) : kept)
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      for_ (suspended attrs) \s -> do
        push $ PlaceInvestigator s.investigator (AtLocation attrs.id)
        resumeActions (attrs.ability 2) s
      pure . RitualCircle_224 $ attrs & setMeta ([] :: [Suspension])
    _ -> RitualCircle_224 <$> liftRunMessage msg attrs
