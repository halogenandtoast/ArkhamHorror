module Arkham.Asset.Cards.OldBookOfLore (
  OldBookOfLore (..),
  oldBookOfLore,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype OldBookOfLore = OldBookOfLore AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldBookOfLore :: AssetCard OldBookOfLore
oldBookOfLore = asset OldBookOfLore Cards.oldBookOfLore

instance HasAbilities OldBookOfLore where
  getAbilities (OldBookOfLore a) =
    [ withCriteria (mkAbility a 1 (ActionAbility Nothing $ Costs [ActionCost 1, exhaust a]))
        $ ControlsThis
          <> InvestigatorExists
            ( InvestigatorAt YourLocation
                <> InvestigatorWithoutModifier CannotManipulateDeck
            )
    ]

instance RunMessage OldBookOfLore where
  runMessage msg a@(OldBookOfLore attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      investigators <- selectList $ colocatedWith iid
      push
        $ chooseOne iid
        $ [ targetLabel iid'
            $ [ Search iid' (toAbilitySource attrs 1) (toTarget iid') [fromTopOfDeck 3] AnyCard (DrawFound iid' 1)
              ]
          | iid' <- investigators
          ]
      pure a
    _ -> OldBookOfLore <$> runMessage msg attrs
