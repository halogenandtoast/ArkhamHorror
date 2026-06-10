module Arkham.Asset.Assets.JohnAndJessieBurke (johnAndJessieBurke) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype JohnAndJessieBurke = JohnAndJessieBurke AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

johnAndJessieBurke :: AssetCard JohnAndJessieBurke
johnAndJessieBurke = ally JohnAndJessieBurke Cards.johnAndJessieBurke (4, 2)

instance HasAbilities JohnAndJessieBurke where
  getAbilities (JohnAndJessieBurke a) =
    [ controlled a 1 (exists $ EnemyAt $ connectedFrom YourLocation)
        $ actionAbilityWithCost (exhaust a <> DamageCost (toSource a) (toTarget a) 1)
    ]

instance RunMessage JohnAndJessieBurke where
  runMessage msg a@(JohnAndJessieBurke attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt $ connectedFrom $ locationWithInvestigator iid
      chooseTargetM iid enemies \enemy -> handleTarget iid (attrs.ability 1) enemy
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (EnemyTarget enemy) -> do
      whenJustM (selectOne $ LocationWithEnemy $ EnemyWithId enemy) \loc -> do
        moveTo (attrs.ability 1) iid loc
        engageEnemy iid enemy
        nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 enemy
      pure a
    _ -> JohnAndJessieBurke <$> liftRunMessage msg attrs
