module Arkham.Enemy.Cards.SupplicantOfTheGoatCircusExMortis (supplicantOfTheGoatCircusExMortis) where

import Arkham.Ability
import Arkham.Campaigns.CircusExMortis.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.ForMovement
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SupplicantOfTheGoatCircusExMortis = SupplicantOfTheGoatCircusExMortis EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

supplicantOfTheGoatCircusExMortis :: EnemyCard SupplicantOfTheGoatCircusExMortis
supplicantOfTheGoatCircusExMortis =
  enemyWith SupplicantOfTheGoatCircusExMortis Cards.supplicantOfTheGoatCircusExMortis
    $ spawnAtL
    ?~ SpawnAt (ConnectedLocation NotForMovement)

instance HasAbilities SupplicantOfTheGoatCircusExMortis where
  getAbilities (SupplicantOfTheGoatCircusExMortis a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #when You (be a)

instance RunMessage SupplicantOfTheGoatCircusExMortis where
  runMessage msg e@(SupplicantOfTheGoatCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ campaignI18n do
        labeled' "supplicantOfTheGoat.seal" do
          selectOne (chaosToken_ #moon) >>= traverse_ (sealChaosToken iid iid)
        labeled' "supplicantOfTheGoat.attack" $ initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> SupplicantOfTheGoatCircusExMortis <$> liftRunMessage msg attrs
