module Arkham.Homebrew.AgesUnwound.Locations.RitualCircle_224 (ritualCircle_224) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement

newtype RitualCircle_224 = RitualCircle_224 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /A Harnessed Future/. Set aside; Scenario III puts it into play.
ritualCircle_224 :: LocationCard RitualCircle_224
ritualCircle_224 = symbolLabel $ location RitualCircle_224 Cards.ritualCircle_224 4 (PerPlayer 1)

{- | The investigators this location has taken out of the game, pending their
return at the start of the next investigation phase.
-}
suspended :: LocationAttrs -> [InvestigatorId]
suspended = getLocationMetaDefault []

{- | While an investigator is out of the game, nothing may reach them. Four core
loops iterate investigators without checking placement, so the ban is spelled
out as modifiers (the approach recorded in
@docs/homebrew/data/ages-unwound-engine-notes.md@ for out-of-play seats).
-}
instance HasModifiersFor RitualCircle_224 where
  getModifiersFor (RitualCircle_224 a) =
    for_ (suspended a) \iid ->
      modified_
        a
        iid
        [ CannotDrawCards
        , CannotGainResources
        , CannotTakeAction IsAnyAction
        , CannotPlay AnyCard
        , CannotMove
        , CannotBeAttacked
        , CannotBeEngaged
        ]

{- | "Haunted - Suspend your current turn and remove your investigator from the
game. At the start of the next investigator phase, return your investigator to
this location and resume the current turn. /
Investigators at this location cannot willingly end their turn while they have
actions remaining."

Ability 2 is the engine hook that brings them back, so it is silent -- the card
prints no second ability.

TODO(ages-unwound): two parts have no primitive and are reported rather than
faked.
  * /resume the current turn/: there is no turn suspension. The investigator
    takes their ordinary turn in the next investigation phase instead of
    continuing this one with whatever actions were left. Restoring the leftover
    count here does not work either -- @Begin InvestigationPhase@ re-derives
    @remainingActions@ and discards accumulated @GainActions@.
  * /cannot willingly end their turn while they have actions remaining/:
    @handlePlayerWindow@ appends @EndTurnButton iid [ChooseEndTurn iid]@
    unconditionally (@Investigator/Runner/Action.hs:564@) and reads no modifier,
    so this needs a core @CannotEndTurn@-style modifier.
-}
instance HasAbilities RitualCircle_224 where
  getAbilities (RitualCircle_224 a) =
    extendRevealed a
      $ campaignI18n (hauntedI "ritualCircleAHarnessedFuture.haunted" a 1)
      : [ mkAbility a 2 $ SilentForcedAbility $ PhaseBegins #when #investigation
        | notNull (suspended a)
        ]

instance RunMessage RitualCircle_224 where
  runMessage msg (RitualCircle_224 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      setActions iid (attrs.ability 1) 0
      endYourTurn iid
      push $ PlaceInvestigator iid Unplaced
      pure . RitualCircle_224 $ attrs & setMeta (nub $ iid : suspended attrs)
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      for_ (suspended attrs) \iid -> push $ PlaceInvestigator iid (AtLocation attrs.id)
      pure . RitualCircle_224 $ attrs & setMeta ([] :: [InvestigatorId])
    _ -> RitualCircle_224 <$> liftRunMessage msg attrs
