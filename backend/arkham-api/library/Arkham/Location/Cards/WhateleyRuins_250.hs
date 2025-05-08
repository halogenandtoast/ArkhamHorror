module Arkham.Location.Cards.WhateleyRuins_250 (whateleyRuins_250) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Projection ()
import Arkham.Location.Cards qualified as Cards (whateleyRuins_250)
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner (locationEnemiesWithTrait, locationInvestigatorsWithClues)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Trait

newtype WhateleyRuins_250 = WhateleyRuins_250 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whateleyRuins_250 :: LocationCard WhateleyRuins_250
whateleyRuins_250 = location WhateleyRuins_250 Cards.whateleyRuins_250 3 (PerPlayer 2)

instance HasModifiersFor WhateleyRuins_250 where
  getModifiersFor (WhateleyRuins_250 attrs) = do
    modifySelect attrs (investigatorAt attrs) [SkillModifier #willpower (-1)]

instance HasAbilities WhateleyRuins_250 where
  getAbilities (WhateleyRuins_250 a) =
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

instance RunMessage WhateleyRuins_250 where
  runMessage msg l@(WhateleyRuins_250 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withClues <- locationInvestigatorsWithClues attrs
      chooseTargetM iid withClues $ handleTarget iid (attrs.ability 1)
      pure l
    HandleTargetChoice _ (isAbilitySource attrs 1 -> True) (InvestigatorTarget iid) -> do
      total <- iid.clues
      scenarioI18n $ chooseAmount' iid "cluesToSpend" "clues" 0 (min 3 total) attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      abominations <- locationEnemiesWithTrait attrs Abomination
      chooseTargetM iid abominations \target -> moveTokens (attrs.ability 1) iid target #clue n
      pure l
    _ -> WhateleyRuins_250 <$> liftRunMessage msg attrs
