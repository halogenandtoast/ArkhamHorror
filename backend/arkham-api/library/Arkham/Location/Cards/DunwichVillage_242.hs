module Arkham.Location.Cards.DunwichVillage_242 (dunwichVillage_242) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards (dunwichVillage_242)
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner (locationInvestigatorsWithClues)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Trait

newtype DunwichVillage_242 = DunwichVillage_242 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dunwichVillage_242 :: LocationCard DunwichVillage_242
dunwichVillage_242 =
  location DunwichVillage_242 Cards.dunwichVillage_242 3 (Static 1)

instance HasAbilities DunwichVillage_242 where
  getAbilities (DunwichVillage_242 a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "dunwichVillage.resign" $ locationResignAction a
      , groupLimit PerGame
          $ restricted a 1 (Here <> youExist InvestigatorWithAnyClues <> exists (EnemyWithTrait Abomination))
          $ FastAbility Free
      ]

instance RunMessage DunwichVillage_242 where
  runMessage msg l@(DunwichVillage_242 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- locationInvestigatorsWithClues attrs
      abominations <- select $ EnemyWithTrait Abomination
      chooseOneM iid $ scenarioI18n do
        countVar 1 $ questionLabeled' "chooseInvestigatorToPlaceClue"
        targets investigators \iid' -> do
          chooseOrRunOneM iid' do
            targets abominations \eid -> moveTokens (attrs.ability 1) iid eid #clue 1
      pure l
    _ -> DunwichVillage_242 <$> liftRunMessage msg attrs
