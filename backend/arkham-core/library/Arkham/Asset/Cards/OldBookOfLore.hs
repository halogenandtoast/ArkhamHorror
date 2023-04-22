module Arkham.Asset.Cards.OldBookOfLore
  ( OldBookOfLore(..)
  , oldBookOfLore
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
    [ restrictedAbility
          a
          1
          (ControlsThis <> InvestigatorExists
            (InvestigatorAt YourLocation
            <> InvestigatorWithoutModifier CannotManipulateDeck
            )
          )
        $ ActionAbility Nothing
        $ Costs [ActionCost 1, ExhaustCost $ toTarget a]
    ]

instance RunMessage OldBookOfLore where
  runMessage msg a@(OldBookOfLore attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      investigatorIds <- selectList $ colocatedWith iid
      push $ chooseOne
        iid
        [ targetLabel
            iid'
            [ Search
                iid'
                source
                (InvestigatorTarget iid')
                [fromTopOfDeck 3]
                AnyCard
                (DrawFound iid' 1)
            ]
        | iid' <- investigatorIds
        ]
      pure a
    _ -> OldBookOfLore <$> runMessage msg attrs
