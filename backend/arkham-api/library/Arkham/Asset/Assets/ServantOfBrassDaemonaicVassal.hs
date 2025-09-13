module Arkham.Asset.Assets.ServantOfBrassDaemonaicVassal (servantOfBrassDaemonaicVassal) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Card
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Message qualified as Msg

newtype ServantOfBrassDaemonaicVassal = ServantOfBrassDaemonaicVassal AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfBrassDaemonaicVassal :: AssetCard ServantOfBrassDaemonaicVassal
servantOfBrassDaemonaicVassal = assetWith ServantOfBrassDaemonaicVassal Cards.servantOfBrassDaemonaicVassal $ healthL ?~ 4

instance HasAbilities ServantOfBrassDaemonaicVassal where
  getAbilities (ServantOfBrassDaemonaicVassal x) =
    [ controlled x 1 (DuringSkillTest SkillTestAtYourLocation)
        $ freeReaction
        $ RevealChaosToken #when Anyone #curse
    , restricted x 2 ControlsThis $ forced $ AssetLeavesPlay #when (be x)
    ]

instance RunMessage ServantOfBrassDaemonaicVassal where
  runMessage msg a@(ServantOfBrassDaemonaicVassal attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOneToHandle iid (attrs.ability 1)
        $ EnemyAt (oneOf [locationWithInvestigator iid, connectedFrom (locationWithInvestigator iid)])
        <> EnemyCanBeDamagedBySource (attrs.ability 1)
      push $ Msg.DealAssetDamage attrs.id (attrs.ability 1) 1 0
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (EnemyTarget eid) -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 2 eid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ PlaceInBonded iid (toCard attrs)
      pure a
    _ -> ServantOfBrassDaemonaicVassal <$> liftRunMessage msg attrs
