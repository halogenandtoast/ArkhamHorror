module Arkham.Location.Cards.DevilsHopYard_253 (devilsHopYard_253) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards (devilsHopYard_253)
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner (locationEnemiesWithTrait, locationInvestigatorsWithClues)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Trait

newtype DevilsHopYard_253 = DevilsHopYard_253 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilsHopYard_253 :: LocationCard DevilsHopYard_253
devilsHopYard_253 = location DevilsHopYard_253 Cards.devilsHopYard_253 2 (PerPlayer 1)

instance HasAbilities DevilsHopYard_253 where
  getAbilities (DevilsHopYard_253 a) =
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

instance RunMessage DevilsHopYard_253 where
  runMessage msg l@(DevilsHopYard_253 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- locationInvestigatorsWithClues attrs
      abominations <- locationEnemiesWithTrait attrs Abomination
      withI18n $ chooseSomeM' iid "done" $ scenarioI18n do
        countVar 1 $ questionLabeled' "chooseInvestigatorToPlaceClue"
        targets investigators \iid' -> do
          chooseOrRunOneM iid' do
            targets abominations \eid -> moveTokens (attrs.ability 1) iid eid #clue 1
      pure l
    _ -> DevilsHopYard_253 <$> liftRunMessage msg attrs
