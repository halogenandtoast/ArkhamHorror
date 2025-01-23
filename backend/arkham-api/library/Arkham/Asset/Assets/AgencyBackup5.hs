module Arkham.Asset.Assets.AgencyBackup5 (agencyBackup5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Message.Lifted.Choose

newtype AgencyBackup5 = AgencyBackup5 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agencyBackup5 :: AssetCard AgencyBackup5
agencyBackup5 = ally AgencyBackup5 Cards.agencyBackup5 (4, 4)

instance HasModifiersFor AgencyBackup5 where
  getModifiersFor (AgencyBackup5 a) = for_ a.controller \iid -> do
      modifySelect
        a
        (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
        [CanAssignDamageToAsset (toId a), CanAssignHorrorToAsset (toId a)]

instance HasAbilities AgencyBackup5 where
  getAbilities (AgencyBackup5 a) =
    [ withTooltip
        "{fast} Exhaust Agency Backup and deal 1 damage to it: Deal 1 damage to an enemy at your location."
        $ fastAbility a 1 (exhaust a <> damageCost a 1)
        $ ControlsThis
        <> exists (at_ YourLocation <> EnemyCanBeDamagedBySource (toSource a))
        <> CanDealDamage
    , withTooltip
        "{fast} Exhaust Agency Backup and deal 1 horror to it: Discover 1 clue at your location."
        $ fastAbility a 2 (exhaust a <> horrorCost a 1)
        $ ControlsThis
        <> CanDiscoverCluesAt YourLocation
        <> OnLocation LocationWithAnyClues
    ]

instance RunMessage AgencyBackup5 where
  runMessage msg a@(AgencyBackup5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      xs <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource source
      chooseTargetM iid xs $ nonAttackEnemyDamage source 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 2) 1
      pure a
    _ -> AgencyBackup5 <$> liftRunMessage msg attrs
