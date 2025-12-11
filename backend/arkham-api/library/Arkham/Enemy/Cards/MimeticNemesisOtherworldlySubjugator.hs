module Arkham.Enemy.Cards.MimeticNemesisOtherworldlySubjugator (mimeticNemesisOtherworldlySubjugator) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Window (damagedEnemyAmount)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MimeticNemesisOtherworldlySubjugator = MimeticNemesisOtherworldlySubjugator EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mimeticNemesisOtherworldlySubjugator :: EnemyCard MimeticNemesisOtherworldlySubjugator
mimeticNemesisOtherworldlySubjugator = enemyWith
  MimeticNemesisOtherworldlySubjugator
  Cards.mimeticNemesisOtherworldlySubjugator
  (3, Static 1, 4)
  (1, 1)
  \a -> a {enemyHealth = Nothing}

instance HasAbilities MimeticNemesisOtherworldlySubjugator where
  getAbilities (MimeticNemesisOtherworldlySubjugator a) =
    extend
      a
      [mkAbility a 1 $ forced $ EnemyWouldTakeDamage #when (be a)]

instance RunMessage MimeticNemesisOtherworldlySubjugator where
  runMessage msg e@(MimeticNemesisOtherworldlySubjugator attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (damagedEnemyAmount -> n) _ -> do
      hollows <- select $ HollowedCard <> basic (not_ $ cardIs Assets.theRedGlovedManHeWasAlwaysThere)
      chooseNM iid n $ targets hollows $ discardCard iid (attrs.ability 1)
      pure e
    _ -> MimeticNemesisOtherworldlySubjugator <$> liftRunMessage msg attrs
