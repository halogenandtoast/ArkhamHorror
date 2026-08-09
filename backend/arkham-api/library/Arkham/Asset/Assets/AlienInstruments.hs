module Arkham.Asset.Assets.AlienInstruments (alienInstruments) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyEvade))
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype AlienInstruments = AlienInstruments AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienInstruments :: AssetCard AlienInstruments
alienInstruments = asset AlienInstruments Cards.alienInstruments

instance HasAbilities AlienInstruments where
  getAbilities (AlienInstruments a) =
    [controlled a 1 (exists $ ExhaustedEnemy <> EnemyAt YourLocation) investigateAction_]

instance RunMessage AlienInstruments where
  runMessage msg a@(AlienInstruments attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ ExhaustedEnemy <> EnemyAt (locationWithInvestigator iid)
      chooseTargetM iid enemies \enemy -> do
        sid <- getRandom
        beginSkillTest
          sid
          iid
          (attrs.ability 1)
          enemy
          #intellect
          (EnemyMaybeFieldCalculation enemy EnemyEvade)
      pure a
    Successful (Action.Investigate, _) iid (isAbilitySource attrs 1 -> True) (EnemyTarget enemy) _ -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 enemy
      withLocationOf iid $ discoverAt IsInvestigate iid (attrs.ability 1) 2
      pure a
    _ -> AlienInstruments <$> liftRunMessage msg attrs
