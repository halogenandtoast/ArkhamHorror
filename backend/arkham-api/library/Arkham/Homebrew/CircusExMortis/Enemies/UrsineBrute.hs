module Arkham.Homebrew.CircusExMortis.Enemies.UrsineBrute (ursineBrute) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Woods))

newtype UrsineBrute = UrsineBrute EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ursineBrute :: EnemyCard UrsineBrute
ursineBrute =
  enemy UrsineBrute Cards.ursineBrute
    & setSpawnAt (LocationWithTrait Woods)

instance HasModifiersFor UrsineBrute where
  getModifiersFor (UrsineBrute a) = modifySelf a [AddKeyword Keyword.Hunter]

instance HasAbilities UrsineBrute where
  getAbilities (UrsineBrute a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyTakeDamage #after AnyDamageEffect (be a) (atLeast 1) AnySource

instance RunMessage UrsineBrute where
  runMessage msg e@(UrsineBrute attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      ready attrs
      pure e
    _ -> UrsineBrute <$> liftRunMessage msg attrs
