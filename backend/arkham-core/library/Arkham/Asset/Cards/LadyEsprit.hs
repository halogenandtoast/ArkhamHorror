module Arkham.Asset.Cards.LadyEsprit (
  LadyEsprit (..),
  ladyEsprit,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype LadyEsprit = LadyEsprit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ladyEsprit :: AssetCard LadyEsprit
ladyEsprit = allyWith LadyEsprit Cards.ladyEsprit (2, 4) (isStoryL .~ True)

instance HasAbilities LadyEsprit where
  getAbilities (LadyEsprit x) =
    [ restrictedAbility
        x
        1
        ( OnSameLocation
            <> InvestigatorExists
              ( AnyInvestigator
                  [ HealableInvestigator (toSource x) DamageType You
                  , You <> InvestigatorCanGainResources
                  ]
              )
        )
        ( ActionAbility Nothing $
            Costs
              [ ActionCost 1
              , ExhaustCost (toTarget x)
              , HorrorCost (toSource x) (toTarget x) 1
              ]
        )
    ]

instance RunMessage LadyEsprit where
  runMessage msg a@(LadyEsprit attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      canHeal <- canHaveDamageHealed attrs iid
      canGainResources <- withoutModifier (InvestigatorTarget iid) CannotGainResources
      push $
        chooseOne iid $
          [ ComponentLabel
            (InvestigatorComponent iid DamageToken)
            [HealDamage (InvestigatorTarget iid) (toSource attrs) 2]
          | canHeal
          ]
            <> [ ComponentLabel
                (InvestigatorComponent iid ResourceToken)
                [TakeResources iid 2 (toAbilitySource attrs 1) False]
               | canGainResources
               ]
      pure a
    _ -> LadyEsprit <$> runMessage msg attrs
