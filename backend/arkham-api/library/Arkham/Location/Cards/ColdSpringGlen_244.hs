module Arkham.Location.Cards.ColdSpringGlen_244 (coldSpringGlen_244) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (coldSpringGlen_244)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Trait

newtype ColdSpringGlen_244 = ColdSpringGlen_244 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldSpringGlen_244 :: LocationCard ColdSpringGlen_244
coldSpringGlen_244 = location ColdSpringGlen_244 Cards.coldSpringGlen_244 3 (Static 2)

instance HasModifiersFor ColdSpringGlen_244 where
  getModifiersFor (ColdSpringGlen_244 attrs) = modifySelect attrs (enemyAt attrs) [EnemyEvade (-1)]

instance HasAbilities ColdSpringGlen_244 where
  getAbilities (ColdSpringGlen_244 a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> exists (InvestigatorWithAnyClues <> at_ (be a))
            <> exists (at_ (be a) <> EnemyWithTrait Abomination)
        )
        (FastAbility Free)

instance RunMessage ColdSpringGlen_244 where
  runMessage msg l@(ColdSpringGlen_244 attrs) = runQueueT $ case msg of
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
    _ -> ColdSpringGlen_244 <$> liftRunMessage msg attrs
