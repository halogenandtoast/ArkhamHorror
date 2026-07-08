module Arkham.Asset.Assets.TheGreatWorkDivideAndUnite (theGreatWorkDivideAndUnite) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (controllerGets, modified_)
import Arkham.Helpers.Scenario (getIsPrelude)
import Arkham.Modifier

newtype TheGreatWorkDivideAndUnite = TheGreatWorkDivideAndUnite AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWorkDivideAndUnite :: AssetCard TheGreatWorkDivideAndUnite
theGreatWorkDivideAndUnite = asset TheGreatWorkDivideAndUnite Cards.theGreatWorkDivideAndUnite

-- Static modifiers rather than a one-shot on CardEnteredPlay: FHV preludes
-- chain into their scenario with investigator setup skipped, so the asset
-- never re-enters play there and a CardEnteredPlay-based modifier would be
-- lost. Deriving these each scenario keeps the +1 XP applied whenever the
-- asset is in play, prelude-reached or not.
instance HasModifiersFor TheGreatWorkDivideAndUnite where
  getModifiersFor (TheGreatWorkDivideAndUnite a) = do
    controllerGets a [BecomeHomunculusWhenDefeated]
    for_ a.controller \iid -> do
      -- "after each scenario": award once per real scenario. Preludes are
      -- skipped so a prelude and the scenario it leads into don't both count.
      isPrelude <- getIsPrelude
      modified_ a iid $ do
        guard (not isPrelude)
        pure $ XPModifier "The Great Work" 1

instance RunMessage TheGreatWorkDivideAndUnite where
  runMessage msg (TheGreatWorkDivideAndUnite attrs) =
    TheGreatWorkDivideAndUnite <$> runMessage msg attrs
