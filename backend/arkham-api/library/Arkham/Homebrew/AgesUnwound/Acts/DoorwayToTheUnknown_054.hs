module Arkham.Homebrew.AgesUnwound.Acts.DoorwayToTheUnknown_054 (doorwayToTheUnknown_054) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Asset.Types (Field (AssetResources))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Helpers (recordTheTimeFor)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype DoorwayToTheUnknown_054 = DoorwayToTheUnknown_054 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doorwayToTheUnknown_054 :: ActCard DoorwayToTheUnknown_054
doorwayToTheUnknown_054 = act (2, A) DoorwayToTheUnknown_054 Cards.doorwayToTheUnknown_054 Nothing

{- | "__Objective__ - If there are 4 resources on Unstable Warding, you must
immediately advance."

This act has no @advanceCost@, so its abilities are a bare list: 'actAbilities1'
would be wrong here. The helper exists to re-publish the clue-spending advance
that 'ActAttrs' only provides when there IS an advance cost; on an act without
one it adds nothing and only drags in a side guard the act does not want.

'onlyOnce' is what keeps the Objective from repeating: the criterion stays true
across the flip, because the warding is not removed from the game until further
down the advance body and that removal is still queued. Without it the Forced
fires again and the second pass crashes on
@placeSetAsideLocation ritualCircle_224@, the first having already taken it out
of the set-aside zone.
-}
instance HasAbilities DoorwayToTheUnknown_054 where
  getAbilities (DoorwayToTheUnknown_054 a) =
    [onlyOnce $ restricted a 1 (exists wardedDoorIsSpent) $ Objective $ forced AnyWindow]

wardedDoorIsSpent :: AssetMatcher
wardedDoorIsSpent =
  assetIs Assets.unstableWarding <> AssetWithTokens (AtLeast $ Static 4) #resource

instance RunMessage DoorwayToTheUnknown_054 where
  runMessage msg a@(DoorwayToTheUnknown_054 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      wardSpent <- selectAny wardedDoorIsSpent

      when wardSpent do
        record TheInvestigatorsDispelledTheWard
        recordTheTimeFor TheInvestigatorsDispelledTheWard
        selectEach (assetIs Assets.unstableWarding) removeFromGame

      ritualCircle <- placeSetAsideLocation Locations.ritualCircle_224

      whenM (getHasRecord TheInvestigatorsUnleashedChaos) do
        extra <- perPlayer 4
        placeTokens attrs ritualCircle #clue extra

        perInvestigator <- perPlayer 1
        resources <- selectSum AssetResources (assetIs Assets.unstableWarding)
        removeTokens attrs ritualCircle #clue (perInvestigator * resources)

      createSetAsideEnemy_ Enemies.theMyriadGentleman_233 ritualCircle

      advanceToAct attrs Cards.breakingTheCircle A
      pure a
    _ -> DoorwayToTheUnknown_054 <$> liftRunMessage msg attrs
