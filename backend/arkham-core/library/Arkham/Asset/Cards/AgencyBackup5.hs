module Arkham.Asset.Cards.AgencyBackup5 (agencyBackup5, AgencyBackup5 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Discover
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype AgencyBackup5 = AgencyBackup5 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

agencyBackup5 :: AssetCard AgencyBackup5
agencyBackup5 = ally AgencyBackup5 Cards.agencyBackup5 (4, 4)

instance HasModifiersFor AgencyBackup5 where
  getModifiersFor (InvestigatorTarget iid) (AgencyBackup5 a) | not (controlledBy a iid) = do
    location <- field InvestigatorLocation iid
    assetLocation <- field AssetLocation (toId a)
    pure $ do
      guard $ isJust location
      guard $ location == assetLocation
      toModifiers a [CanAssignDamageToAsset (toId a), CanAssignHorrorToAsset (toId a)]
  getModifiersFor _ _ = pure []

instance HasAbilities AgencyBackup5 where
  getAbilities (AgencyBackup5 a) =
    [ withTooltip
        "{fast} Exhaust Agency Backup and deal 1 damage to it: Deal 1 damage to an enemy at your location."
        $ fastAbility a 1 (exhaust a <> DamageCost (toSource a) (toTarget a) 1)
        $ ControlsThis
        <> exists (at_ YourLocation <> EnemyCanBeDamagedBySource (toSource a))
        <> CanDealDamage
    , withTooltip
        "{fast} Exhaust Agency Backup and deal 1 horror to it: Discover 1 clue at your location."
        $ fastAbility a 2 (exhaust a <> HorrorCost (toSource a) (toTarget a) 1)
        $ ControlsThis
        <> CanDiscoverCluesAt YourLocation
        <> OnLocation LocationWithAnyClues
    ]

instance RunMessage AgencyBackup5 where
  runMessage msg a@(AgencyBackup5 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      targets <- selectList $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource source
      player <- getPlayer iid
      push
        $ chooseOne player [targetLabel target [EnemyDamage target $ nonAttack source 1] | target <- targets]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ discoverAtYourLocation iid (attrs.ability 2) 1
      pure a
    _ -> AgencyBackup5 <$> runMessage msg attrs
