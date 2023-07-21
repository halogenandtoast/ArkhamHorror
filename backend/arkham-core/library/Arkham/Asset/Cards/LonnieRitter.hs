module Arkham.Asset.Cards.LonnieRitter (
  lonnieRitter,
  LonnieRitter (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Trait (Trait (Item))

newtype LonnieRitter = LonnieRitter AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lonnieRitter :: AssetCard LonnieRitter
lonnieRitter = ally LonnieRitter Cards.lonnieRitter (2, 3)

instance HasModifiersFor LonnieRitter where
  getModifiersFor (InvestigatorTarget iid) (LonnieRitter a)
    | controlledBy a iid =
        pure $ toModifiers a [SkillModifier SkillCombat 1]
  getModifiersFor _ _ = pure []

instance HasAbilities LonnieRitter where
  getAbilities (LonnieRitter a) =
    [ restrictedAbility
        a
        1
        ( ControlsThis
            <> AssetExists
              ( HealableAsset
                  (toSource a)
                  DamageType
                  (AssetWithTrait Item <> AssetControlledBy (InvestigatorAt YourLocation))
              )
        )
        $ FastAbility
        $ ExhaustCost (toTarget a) <> ResourceCost 1
    ]

instance RunMessage LonnieRitter where
  runMessage msg a@(LonnieRitter attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      items <-
        selectList $
          HealableAsset (toSource attrs) DamageType $
            AssetWithTrait Item <> AssetControlledBy (InvestigatorAt $ locationWithInvestigator iid)
      canHealHorror <- selectAny $ HealableAsset (toSource attrs) HorrorType (AssetWithId $ toId attrs)
      push $
        chooseOne
          iid
          [ targetLabel item $
            HealDamage (AssetTarget item) (toSource attrs) 1
              : [HealHorror (toTarget attrs) (toSource attrs) 1 | canHealHorror]
          | item <- items
          ]
      pure a
    _ -> LonnieRitter <$> runMessage msg attrs
