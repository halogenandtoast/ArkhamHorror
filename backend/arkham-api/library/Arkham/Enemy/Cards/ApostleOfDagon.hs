module Arkham.Enemy.Cards.ApostleOfDagon (apostleOfDagon, ApostleOfDagon (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Key
import Arkham.Matcher

newtype ApostleOfDagon = ApostleOfDagon EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apostleOfDagon :: EnemyCard ApostleOfDagon
apostleOfDagon = enemy ApostleOfDagon Cards.apostleOfDagon (2, Static 3, 2) (1, 1)

instance HasModifiersFor ApostleOfDagon where
  getModifiersFor (ApostleOfDagon a) = do
    healthModifier <- perPlayer 1
    modifySelf a [HealthModifier healthModifier]

instance HasAbilities ApostleOfDagon where
  getAbilities (ApostleOfDagon a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemySpawns #after Anywhere $ be a
      , restricted a 2 HasRemainingCurseTokens
          $ forced
          $ EnemyDealtDamage #when AnyDamageEffect (be a) (SourceOwnedBy Anyone)
      ]

instance RunMessage ApostleOfDagon where
  runMessage msg e@(ApostleOfDagon attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeKey attrs BlackKey
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      addChaosToken #curse
      pure e
    _ -> ApostleOfDagon <$> liftRunMessage msg attrs
