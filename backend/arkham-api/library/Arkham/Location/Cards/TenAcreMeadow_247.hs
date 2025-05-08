module Arkham.Location.Cards.TenAcreMeadow_247 (tenAcreMeadow_247) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards (tenAcreMeadow_247)
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner (locationEnemiesWithTrait, locationInvestigatorsWithClues)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Trait

newtype TenAcreMeadow_247 = TenAcreMeadow_247 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tenAcreMeadow_247 :: LocationCard TenAcreMeadow_247
tenAcreMeadow_247 = location TenAcreMeadow_247 Cards.tenAcreMeadow_247 2 (Static 3)

instance HasAbilities TenAcreMeadow_247 where
  getAbilities (TenAcreMeadow_247 a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        ( Here
            <> exists (at_ (be a) <> InvestigatorWithAnyClues)
            <> exists (at_ (be a) <> EnemyWithTrait Abomination)
        )
        (FastAbility Free)

instance RunMessage TenAcreMeadow_247 where
  runMessage msg l@(TenAcreMeadow_247 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- locationInvestigatorsWithClues attrs
      abominations <- locationEnemiesWithTrait attrs Abomination
      withI18n $ chooseSomeM' iid "done" $ scenarioI18n do
        countVar 1 $ questionLabeled' "chooseInvestigatorToPlaceClue"
        targets investigators \iid' -> do
          chooseOrRunOneM iid' do
            targets abominations \eid -> moveTokens (attrs.ability 1) iid eid #clue 1
      pure l
    _ -> TenAcreMeadow_247 <$> liftRunMessage msg attrs
