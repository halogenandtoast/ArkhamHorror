module Arkham.Enemy.Cards.KhalidBelovedCompanion (khalidBelovedCompanion) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Token

newtype KhalidBelovedCompanion = KhalidBelovedCompanion EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

khalidBelovedCompanion :: EnemyCard KhalidBelovedCompanion
khalidBelovedCompanion = enemy KhalidBelovedCompanion Cards.khalidBelovedCompanion (4, Static 5, 3) (2, 1)

instance HasAbilities KhalidBelovedCompanion where
  getAbilities (KhalidBelovedCompanion a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemySpawns #after (oneOf [LocationWithEnemy NonEliteEnemy, LocationWithToken Civilian]) (be a)

instance RunMessage KhalidBelovedCompanion where
  runMessage msg e@(KhalidBelovedCompanion attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs \lid -> do
        locations <- select $ connectedTo $ LocationWithId lid
        forTargets locations msg
      pure e
    ForTargets ls msg'@(UseThisAbility _ (isSource attrs -> True) 1) -> do
      withLocationOf attrs \lid -> do
        enemies <- select $ enemyAt lid <> NonEliteEnemy <> not_ (EnemyWithId attrs.id)
        civilians <- fieldMap LocationTokens (countTokens Civilian) lid
        unless (null enemies && civilians == 0) do
          lead <- getLead
          let ls' = mapMaybe (preview _LocationTarget) ls
          locations <- if null ls' then select (connectedTo $ LocationWithId lid) else pure ls'

          chooseOneM lead do
            targets enemies \otherEnemy ->
              chooseOneM lead do
                for_ (eachWithRest locations) \(location, rest) -> do
                  targeting location do
                    enemyMoveTo (attrs.ability 1) otherEnemy location
                    forTargets rest msg'
            when (civilians > 0) do
              targeting lid do
                chooseOneM lead do
                  for_ (eachWithRest locations) \(location, rest) -> do
                    targeting location do
                      moveTokens (attrs.ability 1) lid location Civilian 1
                      forTargets rest msg'
      pure e
    _ -> KhalidBelovedCompanion <$> liftRunMessage msg attrs
