module Arkham.Asset.Cards.AbbessAllegriaDiBiase (
  abbessAllegriaDiBiase,
  AbbessAllegriaDiBiase (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection

newtype AbbessAllegriaDiBiase = AbbessAllegriaDiBiase AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abbessAllegriaDiBiase :: AssetCard AbbessAllegriaDiBiase
abbessAllegriaDiBiase = ally AbbessAllegriaDiBiase Cards.abbessAllegriaDiBiase (2, 2)

instance HasAbilities AbbessAllegriaDiBiase where
  getAbilities (AbbessAllegriaDiBiase attrs) =
    [ fastAbility attrs 1 (exhaust attrs) $
        AnyCriterion
          [ OnSameLocation <> LocationExists AccessibleLocation
          , LocationExists $ YourLocation <> ConnectedTo (locationWithAsset attrs)
          ]
    ]

instance RunMessage AbbessAllegriaDiBiase where
  runMessage msg a@(AbbessAllegriaDiBiase attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      location <- getJustLocation iid
      abbessLocation <- fieldJust AssetLocation attrs
      if location == abbessLocation
        then do
          connectedLocations <- selectList $ accessibleFrom location
          push . chooseOrRunOne iid $
            [ targetLabel connectedLocation [Move $ move attrs iid connectedLocation]
            | connectedLocation <- connectedLocations
            ]
        else push $ Move $ move attrs iid abbessLocation
      pure a
    _ -> AbbessAllegriaDiBiase <$> runMessage msg attrs
