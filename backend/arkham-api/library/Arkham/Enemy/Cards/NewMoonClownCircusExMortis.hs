module Arkham.Enemy.Cards.NewMoonClownCircusExMortis (newMoonClownCircusExMortis) where

import Arkham.Ability
import Arkham.Campaigns.CircusExMortis.Helpers (getSealedMoonTokens)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype NewMoonClownCircusExMortis = NewMoonClownCircusExMortis EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newMoonClownCircusExMortis :: EnemyCard NewMoonClownCircusExMortis
newMoonClownCircusExMortis = enemy NewMoonClownCircusExMortis Cards.newMoonClownCircusExMortis

instance HasModifiersFor NewMoonClownCircusExMortis where
  getModifiersFor (NewMoonClownCircusExMortis a) = do
    investigators <- select $ InvestigatorAt (locationWithEnemy a)
    n <- sum <$> traverse (fmap length . getSealedMoonTokens) investigators
    modifySelf a $ [AddKeyword Keyword.Hunter, AddKeyword Keyword.Retaliate] <> [EnemyEvade n | n > 0]

instance HasAbilities NewMoonClownCircusExMortis where
  getAbilities (NewMoonClownCircusExMortis a) =
    extend1 a
      $ mkAbility a 1
      $ freeReaction
      $ SkillTestResult #after You (WhileEvadingAnEnemy $ be a) (SuccessResult $ atLeast 2)

instance RunMessage NewMoonClownCircusExMortis where
  runMessage msg e@(NewMoonClownCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healHorror iid (attrs.ability 1) 1
      shuffleBackIntoEncounterDeck attrs
      pure e
    _ -> NewMoonClownCircusExMortis <$> liftRunMessage msg attrs
