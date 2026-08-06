module Arkham.Asset.Assets.WalkInFaith (walkInFaith) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Campaigns.TheDrownedCity.Helpers (taskEnds)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message.Lifted.Log (incrementRecordCountForInvestigator)

newtype WalkInFaith = WalkInFaith AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

walkInFaith :: AssetCard WalkInFaith
walkInFaith = asset WalkInFaith Cards.walkInFaith

instance HasAbilities WalkInFaith where
  getAbilities (WalkInFaith a) =
    [ controlled a 1 NoRestriction
        $ freeReaction (RevealChaosToken #after Anyone #eldersign)
    , -- "more than 1 [per_investigator] signs" scales with the player count, so
      -- the threshold has to be a calculation rather than a pure token check.
      controlled
        a
        2
        (HasCalculation (AssetTokenCountCalculation a.id #sign) (GreaterThan $ PerPlayer 1))
        $ forced taskEnds
    ]

instance RunMessage WalkInFaith where
  runMessage msg a@(WalkInFaith attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs #sign 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      incrementRecordCountForInvestigator iid Key.WalkInFaith 1
      pure a
    _ -> WalkInFaith <$> liftRunMessage msg attrs
