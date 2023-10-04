module Arkham.Asset.Cards.PatricesViolin (
  patricesViolin,
  PatricesViolin (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype PatricesViolin = PatricesViolin AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patricesViolin :: AssetCard PatricesViolin
patricesViolin = asset PatricesViolin Cards.patricesViolin

instance HasAbilities PatricesViolin where
  getAbilities (PatricesViolin x) =
    [ controlledAbility
        x
        1
        ( exists
            $ InvestigatorAt YourLocation
            <> AnyInvestigator
              [InvestigatorWithoutModifier CannotGainResources, InvestigatorWithoutModifier CannotDrawCards]
        )
        $ FastAbility (exhaust x <> HandDiscardCost 1 AnyCard)
    ]

instance RunMessage PatricesViolin where
  runMessage msg a@(PatricesViolin attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigatorIds <-
        selectList
          $ colocatedWith iid
          <> AnyInvestigator
            [InvestigatorWithoutModifier CannotGainResources, InvestigatorWithoutModifier CannotDrawCards]
      push
        $ chooseOrRunOne
          iid
          [ targetLabel iid' [HandleTargetChoice iid (toAbilitySource attrs 1) (toTarget iid')]
          | iid' <- investigatorIds
          ]
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (InvestigatorTarget iid') -> do
      canGainResources <- withoutModifier iid' CannotGainResources
      canDrawCards <- withoutModifier iid' CannotDrawCards
      drawing <- drawCards iid' (toAbilitySource attrs 1) 1

      push
        $ chooseOne iid
        $ [ Label "Gain resource" [TakeResources iid' 1 (toAbilitySource attrs 1) False]
          | canGainResources
          ]
        <> [Label "Draw card" [drawing] | canDrawCards]

      pure a
    _ -> PatricesViolin <$> runMessage msg attrs
