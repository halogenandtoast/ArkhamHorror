module Arkham.Homebrew.DarkMatter.Assets.GermaniumDetector (germaniumDetector) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (pattern ScanAsIfHere)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GermaniumDetector = GermaniumDetector AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

germaniumDetector :: AssetCard GermaniumDetector
germaniumDetector = asset GermaniumDetector Cards.germaniumDetector

{- | "[action] Exhaust Germanium Detector: Choose any revealed location. That
location gets -2 shroud and investigators may perform Scan abilities as if they
were at that location until the end of the round."
-}
instance HasAbilities GermaniumDetector where
  getAbilities (GermaniumDetector a) =
    [controlled a 1 (exists RevealedLocation) $ actionAbilityWithCost (exhaust a)]

instance RunMessage GermaniumDetector where
  runMessage msg a@(GermaniumDetector attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select RevealedLocation
      chooseTargetM iid locations \lid ->
        roundModifiers (attrs.ability 1) lid [ShroudModifier (-2), ScanAsIfHere]
      pure a
    _ -> GermaniumDetector <$> liftRunMessage msg attrs
