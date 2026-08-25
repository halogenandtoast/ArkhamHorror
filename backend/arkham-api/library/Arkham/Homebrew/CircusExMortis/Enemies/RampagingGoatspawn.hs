module Arkham.Homebrew.CircusExMortis.Enemies.RampagingGoatspawn (rampagingGoatspawn) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (randomDiscardN)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype RampagingGoatspawn = RampagingGoatspawn EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rampagingGoatspawn :: EnemyCard RampagingGoatspawn
rampagingGoatspawn = enemy RampagingGoatspawn Cards.rampagingGoatspawn

instance HasModifiersFor RampagingGoatspawn where
  getModifiersFor (RampagingGoatspawn a) = modifySelf a [AddKeyword Keyword.Massive]

instance HasAbilities RampagingGoatspawn where
  getAbilities (RampagingGoatspawn a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyAttacked #after You AnySource (be a)
      , mkAbility a 2
          $ triggered
            (EnemyTakeDamage #after AnyDamageEffect (be a) (atLeast 1) AnySource)
            (GroupClueCostRange (1, 3) Anywhere)
      ]

instance RunMessage RampagingGoatspawn where
  runMessage msg e@(RampagingGoatspawn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      tokens <- getSealedMoonTokens iid
      unless (null tokens) $ randomDiscardN iid (attrs.ability 1) (length tokens)
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 _ (totalCluePayment -> x) -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 2) x attrs
      pure e
    _ -> RampagingGoatspawn <$> liftRunMessage msg attrs
