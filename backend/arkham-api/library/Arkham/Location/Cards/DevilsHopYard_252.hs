module Arkham.Location.Cards.DevilsHopYard_252 (devilsHopYard_252) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Investigator.Projection ()
import Arkham.Location.Cards qualified as Cards (devilsHopYard_252)
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner (locationEnemiesWithTrait, locationInvestigatorsWithClues)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Trait

newtype DevilsHopYard_252 = DevilsHopYard_252 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilsHopYard_252 :: LocationCard DevilsHopYard_252
devilsHopYard_252 = location DevilsHopYard_252 Cards.devilsHopYard_252 1 (Static 2)

instance HasAbilities DevilsHopYard_252 where
  getAbilities (DevilsHopYard_252 a) =
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

instance RunMessage DevilsHopYard_252 where
  runMessage msg l@(DevilsHopYard_252 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withClues <- locationInvestigatorsWithClues attrs
      chooseTargetM iid withClues $ handleTarget iid (attrs.ability 1)
      pure l
    HandleTargetChoice _ (isAbilitySource attrs 1 -> True) (InvestigatorTarget iid) -> do
      total <- iid.clues
      scenarioI18n $ chooseAmount' iid "cluesToSpend" "clues" 0 (min 2 total) attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      abominations <- locationEnemiesWithTrait attrs Abomination
      chooseTargetM iid abominations \target -> moveTokens (attrs.ability 1) iid target #clue n
      pure l
    _ -> DevilsHopYard_252 <$> liftRunMessage msg attrs
