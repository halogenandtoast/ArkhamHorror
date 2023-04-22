module Arkham.Asset.Cards.AgencyBackup5
  ( agencyBackup5
  , AgencyBackup5(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Projection

newtype AgencyBackup5 = AgencyBackup5 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agencyBackup5 :: AssetCard AgencyBackup5
agencyBackup5 = ally AgencyBackup5 Cards.agencyBackup5 (4, 4)

instance HasModifiersFor AgencyBackup5 where
  getModifiersFor (InvestigatorTarget iid) (AgencyBackup5 a)
    | not (controlledBy a iid) = do
      locationId <- field InvestigatorLocation iid
      assetLocationId <- field AssetLocation (toId a)
      pure
        $ toModifiers a
        $ if (locationId == assetLocationId) && isJust locationId
            then
              [CanAssignDamageToAsset (toId a), CanAssignHorrorToAsset (toId a)]
            else []
  getModifiersFor _ _ = pure []

instance HasAbilities AgencyBackup5 where
  getAbilities (AgencyBackup5 a) =
    [ withTooltip
        "{fast} Exhaust Agency Backup and deal 1 damage to it: Deal 1 damage to an enemy at your location."
      $ restrictedAbility
          a
          1
          (ControlsThis <> EnemyCriteria
            (EnemyExists $ EnemyAt YourLocation <> EnemyCanBeDamagedBySource
              (toSource a)
            )
          )
      $ FastAbility
      $ ExhaustCost (toTarget a)
      <> DamageCost (toSource a) (toTarget a) 1
    , withTooltip
        "{fast} Exhaust Agency Backup and deal 1 horror to it: Discover 1 clue at your location."
      $ restrictedAbility
          a
          2
          (ControlsThis
          <> CanDiscoverCluesAt YourLocation
          <> OnLocation LocationWithAnyClues
          )
      $ FastAbility
      $ ExhaustCost (toTarget a)
      <> HorrorCost (toSource a) (toTarget a) 1
    ]

instance RunMessage AgencyBackup5 where
  runMessage msg a@(AgencyBackup5 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      targets <- selectList $ EnemyAt YourLocation <> EnemyCanBeDamagedBySource
        (toAbilitySource attrs 1)
      push $ chooseOne
        iid
        [ targetLabel target [EnemyDamage target $ nonAttack (toSource attrs) 1]
        | target <- targets
        ]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ InvestigatorDiscoverCluesAtTheirLocation iid 1 Nothing
      pure a
    _ -> AgencyBackup5 <$> runMessage msg attrs
