module Arkham.Homebrew.CircusExMortis.Enemies.TheBlackGoat (theBlackGoat) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheBlackGoat = TheBlackGoat EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackGoat :: EnemyCard TheBlackGoat
theBlackGoat = enemy TheBlackGoat Cards.theBlackGoat

instance HasAbilities TheBlackGoat where
  getAbilities (TheBlackGoat a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacked #when You AnySource (be a)

instance RunMessage TheBlackGoat where
  runMessage msg e@(TheBlackGoat attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sealed <- select $ SealedOnEnemy (be attrs) moonToken
      chooseOneM iid $ campaignI18n $ scope "theBlackGoat" do
        labeledValidate (notNull sealed) "releaseMoonToken"
          $ for_ (listToMaybe sealed) releaseMoonToken
        labeled "readiesAndAttacks" do
          readyThis attrs
          initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> TheBlackGoat <$> liftRunMessage msg attrs
