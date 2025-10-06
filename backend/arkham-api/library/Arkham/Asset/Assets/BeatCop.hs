module Arkham.Asset.Assets.BeatCop (beatCop) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message.Lifted.Choose

newtype BeatCop = BeatCop AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beatCop :: AssetCard BeatCop
beatCop = ally BeatCop Cards.beatCop (2, 2)

instance HasModifiersFor BeatCop where
  getModifiersFor (BeatCop a) = controllerGets a [SkillModifier #combat 1]

instance HasAbilities BeatCop where
  getAbilities (BeatCop x) =
    [ controlled x 1 (canDamageEnemyAt (x.ability 1) YourLocation)
        $ FastAbility (DiscardCost FromPlay $ toTarget x)
    ]

instance RunMessage BeatCop where
  runMessage msg a@(BeatCop attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseDamageEnemy iid (attrs.ability 1) (locationWithInvestigator iid) AnyEnemy 1
      pure a
    _ -> BeatCop <$> liftRunMessage msg attrs
