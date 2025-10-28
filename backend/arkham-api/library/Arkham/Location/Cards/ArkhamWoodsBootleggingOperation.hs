module Arkham.Location.Cards.ArkhamWoodsBootleggingOperation (arkhamWoodsBootleggingOperation) where

import Arkham.Ability
import Arkham.Helpers.Window (getPassedBy)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ArkhamWoodsBootleggingOperation = ArkhamWoodsBootleggingOperation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsBootleggingOperation :: LocationCard ArkhamWoodsBootleggingOperation
arkhamWoodsBootleggingOperation = location ArkhamWoodsBootleggingOperation Cards.arkhamWoodsBootleggingOperation 1 (PerPlayer 1)

instance HasAbilities ArkhamWoodsBootleggingOperation where
  getAbilities (ArkhamWoodsBootleggingOperation a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ SkillTestResult #after You (whileInvestigating a) #success
      , restricted a 2 (exists $ InvestigatorAt (be a)) $ forced EncounterDeckRunsOutOfCards
      ]

instance RunMessage ArkhamWoodsBootleggingOperation where
  runMessage msg l@(ArkhamWoodsBootleggingOperation attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getPassedBy -> n) _ -> do
      discardTopOfEncounterDeck iid (attrs.ability 1) n
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      selectEach (InvestigatorAt (be attrs)) $ assignDamageTo (attrs.ability 2) 2
      pure l
    _ -> ArkhamWoodsBootleggingOperation <$> liftRunMessage msg attrs
