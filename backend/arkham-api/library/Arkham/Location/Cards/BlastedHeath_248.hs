module Arkham.Location.Cards.BlastedHeath_248 (blastedHeath_248) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (blastedHeath_248)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Trait

newtype BlastedHeath_248 = BlastedHeath_248 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blastedHeath_248 :: LocationCard BlastedHeath_248
blastedHeath_248 = location BlastedHeath_248 Cards.blastedHeath_248 4 (Static 3)

instance HasAbilities BlastedHeath_248 where
  getAbilities (BlastedHeath_248 a) =
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

instance RunMessage BlastedHeath_248 where
  runMessage msg l@(BlastedHeath_248 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      total <- selectSum InvestigatorClues (at_ (be attrs) <> InvestigatorWithAnyClues)
      scenarioI18n $ chooseAmount' iid "cluesToSpend" "$clues" 0 (min 2 total) attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      investigators <- select $ at_ (be attrs) <> InvestigatorWithAnyClues
      abominations <- select $ EnemyWithTrait Abomination <> at_ (be attrs)

      chooseTargetM iid abominations \target -> do
        push $ SpendClues n investigators
        placeClues (attrs.ability 1) target n
      pure l
    _ -> BlastedHeath_248 <$> liftRunMessage msg attrs
