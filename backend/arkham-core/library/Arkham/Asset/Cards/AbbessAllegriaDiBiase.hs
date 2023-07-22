module Arkham.Asset.Cards.AbbessAllegriaDiBiase (
  abbessAllegriaDiBiase,
  AbbessAllegriaDiBiase (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher hiding (MoveAction)
import Arkham.Projection

newtype AbbessAllegriaDiBiase = AbbessAllegriaDiBiase AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abbessAllegriaDiBiase :: AssetCard AbbessAllegriaDiBiase
abbessAllegriaDiBiase =
  ally AbbessAllegriaDiBiase Cards.abbessAllegriaDiBiase (2, 2)

instance HasAbilities AbbessAllegriaDiBiase where
  getAbilities (AbbessAllegriaDiBiase attrs) =
    [ restricted
        ( AnyCriterion
            [ OnSameLocation <> LocationExists AccessibleLocation
            , LocationExists $
                YourLocation
                  <> ConnectedTo
                    (locationWithAsset $ toId attrs)
            ]
        )
        $ fastAbility
          attrs
          1
          (ExhaustCost $ toTarget attrs)
    ]

instance RunMessage AbbessAllegriaDiBiase where
  runMessage msg a@(AbbessAllegriaDiBiase attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      locationId <- getJustLocation iid
      abbessLocationId <- fieldJust AssetLocation (toId attrs)
      if locationId == abbessLocationId
        then do
          connectedLocationIds <- selectList $ accessibleFrom locationId
          push $
            chooseOrRunOne
              iid
              [ targetLabel
                connectedLocationId
                [MoveAction iid connectedLocationId Free False]
              | connectedLocationId <- connectedLocationIds
              ]
        else push $ MoveAction iid abbessLocationId Free False
      pure a
    _ -> AbbessAllegriaDiBiase <$> runMessage msg attrs
