module Arkham.Homebrew.CircusExMortis.Enemies.LoomingGoatspawn (loomingGoatspawn) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Enemy.Types.Attrs (enemyDamage)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Projection
import Arkham.Window qualified as Window

newtype LoomingGoatspawn = LoomingGoatspawn EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loomingGoatspawn :: EnemyCard LoomingGoatspawn
loomingGoatspawn = enemy LoomingGoatspawn Cards.loomingGoatspawn

instance HasModifiersFor LoomingGoatspawn where
  getModifiersFor (LoomingGoatspawn a) =
    modifySelf a [AddKeyword Keyword.Massive, AddKeyword Keyword.Alert]

getWouldDamageEnemy :: [Window.Window] -> Maybe (Source, EnemyId, Int)
getWouldDamageEnemy =
  listToMaybe . mapMaybe \case
    (Window.windowType -> Window.WouldTakeDamage source (EnemyTarget eid) n _) -> Just (source, eid, n)
    _ -> Nothing

instance HasAbilities LoomingGoatspawn where
  getAbilities (LoomingGoatspawn a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyWouldTakeDamage #when (SourceUsedBy hasSealedMoonToken) (be a)
      , mkAbility a 2
          $ triggered
            (EnemyTakeDamage #after AnyDamageEffect (be a) (atLeast 1) AnySource)
            (GroupClueCostRange (1, 3) Anywhere)
      ]

instance RunMessage LoomingGoatspawn where
  runMessage msg e@(LoomingGoatspawn attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      for_ (getWouldDamageEnemy ws) \(_, eid, n) -> do
        mHealth <- field EnemyHealth eid
        let lethal = maybe False (\h -> enemyDamage attrs + n >= h) mHealth
        when lethal $ reduceDamageTaken (attrs.ability 1) eid n
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 _ (totalCluePayment -> x) -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 2) x attrs
      pure e
    _ -> LoomingGoatspawn <$> liftRunMessage msg attrs
