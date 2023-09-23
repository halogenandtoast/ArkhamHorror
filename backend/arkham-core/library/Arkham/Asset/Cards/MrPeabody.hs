module Arkham.Asset.Cards.MrPeabody (
  mrPeabody,
  MrPeabody (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher

newtype MrPeabody = MrPeabody AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mrPeabody :: AssetCard MrPeabody
mrPeabody = ally MrPeabody Cards.mrPeabody (2, 2)

instance HasAbilities MrPeabody where
  getAbilities (MrPeabody attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ActionAbility Nothing
        $ Costs
          [ActionCost 1, ExhaustCost $ toTarget attrs]
    ]

instance RunMessage MrPeabody where
  runMessage msg a@(MrPeabody attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      locations <- selectListMap LocationTarget Anywhere
      a
        <$ push
          ( chooseOne
              iid
              [ TargetLabel
                target
                [CreateEffect (toCardCode attrs) Nothing source target]
              | target <- locations
              ]
          )
    _ -> MrPeabody <$> runMessage msg attrs
