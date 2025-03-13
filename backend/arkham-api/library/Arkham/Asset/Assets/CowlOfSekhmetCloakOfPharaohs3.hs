module Arkham.Asset.Assets.CowlOfSekhmetCloakOfPharaohs3 (cowlOfSekhmetCloakOfPharaohs3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Modifier

newtype CowlOfSekhmetCloakOfPharaohs3 = CowlOfSekhmetCloakOfPharaohs3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cowlOfSekhmetCloakOfPharaohs3 :: AssetCard CowlOfSekhmetCloakOfPharaohs3
cowlOfSekhmetCloakOfPharaohs3 = assetWith CowlOfSekhmetCloakOfPharaohs3 Cards.cowlOfSekhmetCloakOfPharaohs3 (sanityL ?~ 2)

instance HasAbilities CowlOfSekhmetCloakOfPharaohs3 where
  getAbilities (CowlOfSekhmetCloakOfPharaohs3 x) =
    [ restricted x 1 ControlsThis
        $ triggered (Enters #when You $ LocationWithEnemy NonEliteEnemy) (exhaust x)
    ]

instance RunMessage CowlOfSekhmetCloakOfPharaohs3 where
  runMessage msg a@(CowlOfSekhmetCloakOfPharaohs3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (entering -> lid) _ -> do
      enemies <- select $ enemyAt lid
      roundModifier (attrs.ability 1) iid (CannotBeEngagedBy $ mapOneOf EnemyWithId enemies)
      for_ enemies \enemy -> do
        roundModifier (attrs.ability 1) enemy
          $ CannotBeDamagedByPlayerSources (SourceOwnedBy $ InvestigatorWithId iid)
      pure a
    _ -> CowlOfSekhmetCloakOfPharaohs3 <$> liftRunMessage msg attrs
