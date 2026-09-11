module Arkham.Homebrew.CircusExMortis.Enemies.SupplicantOfTheGoat (supplicantOfTheGoat) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.ForMovement
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SupplicantOfTheGoat = SupplicantOfTheGoat EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

supplicantOfTheGoat :: EnemyCard SupplicantOfTheGoat
supplicantOfTheGoat =
  enemyWith SupplicantOfTheGoat Cards.supplicantOfTheGoat
    $ spawnAtL
    ?~ SpawnAt (ConnectedLocation NotForMovement)

instance HasAbilities SupplicantOfTheGoat where
  getAbilities (SupplicantOfTheGoat a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #when You (be a)

instance RunMessage SupplicantOfTheGoat where
  runMessage msg e@(SupplicantOfTheGoat attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moonInBag <- selectAny moonToken
      chooseOneM iid $ campaignI18n do
        labeledValidate' moonInBag "sealMoonToken" $ sealMoonTokenOn iid
        unscoped
          $ nameVar attrs
          $ labeled "attacksYou"
          $ initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> SupplicantOfTheGoat <$> liftRunMessage msg attrs
