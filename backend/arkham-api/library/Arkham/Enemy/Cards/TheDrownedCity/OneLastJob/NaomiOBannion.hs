module Arkham.Enemy.Cards.TheDrownedCity.OneLastJob.NaomiOBannion (naomiOBannion) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Helpers (campaignI18n)
import Arkham.Enemy.CardDefs.TheDrownedCity.OneLastJob qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype NaomiOBannion = NaomiOBannion EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

naomiOBannion :: EnemyCard NaomiOBannion
naomiOBannion = enemy NaomiOBannion Cards.naomiOBannion

instance HasModifiersFor NaomiOBannion where
  getModifiersFor (NaomiOBannion attrs) = do
    n <- perPlayer 1
    modifySelf attrs [CannotMove, HealthModifier n]

instance HasAbilities NaomiOBannion where
  getAbilities (NaomiOBannion attrs) =
    extend1 attrs
      $ groupLimit PerRound
      $ mkAbility attrs 1
      $ forced
      $ EnemyEvaded #after You (be attrs)

instance RunMessage NaomiOBannion where
  runMessage msg e@(NaomiOBannion attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ InvestigatorAt $ locationWithEnemy attrs.id
      campaignI18n $ case investigators of
        [] -> readyThis attrs
        [sole] -> chooseOneM iid do
          labeled "naomiOBannion.preventReadying" $ assignDamage sole (attrs.ability 1) 1
          labeled "naomiOBannion.doNotTakeDamage" $ readyThis attrs
        _ -> chooseOneM iid do
          questionLabeled "naomiOBannion.chooseDamage"
          targets investigators \i -> assignDamage i (attrs.ability 1) 1
          labeled "naomiOBannion.doNotTakeDamage" $ readyThis attrs
      pure e
    _ -> NaomiOBannion <$> liftRunMessage msg attrs
