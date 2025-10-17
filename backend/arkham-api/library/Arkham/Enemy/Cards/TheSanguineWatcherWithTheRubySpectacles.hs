module Arkham.Enemy.Cards.TheSanguineWatcherWithTheRubySpectacles (theSanguineWatcherWithTheRubySpectacles) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheSanguineWatcherWithTheRubySpectacles = TheSanguineWatcherWithTheRubySpectacles EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSanguineWatcherWithTheRubySpectacles :: EnemyCard TheSanguineWatcherWithTheRubySpectacles
theSanguineWatcherWithTheRubySpectacles =
  enemy
    TheSanguineWatcherWithTheRubySpectacles
    Cards.theSanguineWatcherWithTheRubySpectacles
    (4, PerPlayer 5, 4)
    (2, 2)

instance HasAbilities TheSanguineWatcherWithTheRubySpectacles where
  getAbilities (TheSanguineWatcherWithTheRubySpectacles a) = extend1 a $ restricted a 1 (thisExists a ReadyEnemy) $ forced $ PhaseEnds #when #enemy

instance RunMessage TheSanguineWatcherWithTheRubySpectacles where
  runMessage msg e@(TheSanguineWatcherWithTheRubySpectacles attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> do
        chooseOneM iid $ withI18n $ countVar 2 do
          labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 2
          labeled' "takeHorror" $ assignHorror iid (attrs.ability 1) 2
      pure e
    _ -> TheSanguineWatcherWithTheRubySpectacles <$> liftRunMessage msg attrs
