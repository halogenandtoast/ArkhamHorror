module Arkham.Homebrew.CircusExMortis.Enemies.NewMoonClown (newMoonClown) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.Helpers (getSealedMoonTokens)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype NewMoonClown = NewMoonClown EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newMoonClown :: EnemyCard NewMoonClown
newMoonClown = enemy NewMoonClown Cards.newMoonClown

instance HasModifiersFor NewMoonClown where
  getModifiersFor (NewMoonClown a) = do
    investigators <- select $ InvestigatorAt (locationWithEnemy a)
    n <- sum <$> traverse (fmap length . getSealedMoonTokens) investigators
    modifySelf a $ [AddKeyword Keyword.Hunter, AddKeyword Keyword.Retaliate] <> [EnemyEvade n | n > 0]

instance HasAbilities NewMoonClown where
  getAbilities (NewMoonClown a) =
    extend1 a
      $ mkAbility a 1
      $ freeReaction
      $ SkillTestResult #after You (WhileEvadingAnEnemy $ be a) (SuccessResult $ atLeast 2)

instance RunMessage NewMoonClown where
  runMessage msg e@(NewMoonClown attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healHorror iid (attrs.ability 1) 1
      shuffleBackIntoEncounterDeck attrs
      pure e
    _ -> NewMoonClown <$> liftRunMessage msg attrs
