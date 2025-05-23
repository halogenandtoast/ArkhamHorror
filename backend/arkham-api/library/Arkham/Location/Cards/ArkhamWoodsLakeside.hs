module Arkham.Location.Cards.ArkhamWoodsLakeside (arkhamWoodsLakeside) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype ArkhamWoodsLakeside = ArkhamWoodsLakeside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsLakeside :: LocationCard ArkhamWoodsLakeside
arkhamWoodsLakeside = location ArkhamWoodsLakeside Cards.arkhamWoodsLakeside 2 (PerPlayer 1)

instance HasAbilities ArkhamWoodsLakeside where
  getAbilities (ArkhamWoodsLakeside a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> DuringSkillTest (WhileInvestigating $ be a))
      $ forced
      $ RevealChaosToken #after You AnyChaosToken

instance RunMessage ArkhamWoodsLakeside where
  runMessage msg l@(ArkhamWoodsLakeside attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawAnotherChaosToken iid
      pure l
    _ -> ArkhamWoodsLakeside <$> liftRunMessage msg attrs
