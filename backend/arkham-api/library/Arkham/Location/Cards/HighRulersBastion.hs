module Arkham.Location.Cards.HighRulersBastion (highRulersBastion) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HighRulersBastion = HighRulersBastion LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highRulersBastion :: LocationCard HighRulersBastion
highRulersBastion = location HighRulersBastion Cards.highRulersBastion 4 (PerPlayer 1)

instance HasAbilities HighRulersBastion where
  getAbilities (HighRulersBastion a) =
    extendRevealed
      a
      [ restricted a 1 (thisExists a LocationNotAtClueLimit) $ forced $ RoundEnds #when
      , limitedAbility (MaxPer Cards.highRulersBastion PerGame 1)
          $ fastAbility a 2 Free
          $ Here
          <> exists (VictoryDisplayCardMatch $ basic $ cardIs Enemies.saturniteMonarchGraciousHost)
      ]

instance RunMessage HighRulersBastion where
  runMessage msg l@(HighRulersBastion attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeCluesUpToClueValue (attrs.ability 1) attrs
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      heliosTelescope <- selectJust $ assetIs Assets.heliosTelescopeGateToTheCosmos
      placeTokens (attrs.ability 2) heliosTelescope Shard 1
      pure l
    _ -> HighRulersBastion <$> liftRunMessage msg attrs
