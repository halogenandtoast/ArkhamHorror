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
    [ fastAbility attrs 1 (exhaust attrs)
        $ AnyCriterion
          [ OnSameLocation <> LocationExists AccessibleLocation
          , LocationExists $ YourLocation <> ConnectedTo (locationWithAsset attrs)
          ]
    ]

instance RunMessage AbbessAllegriaDiBiase where
  runMessage msg a@(AbbessAllegriaDiBiase attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      location <- getJustLocation iid
      abbessLocation <- fieldJust AssetLocation attrs
      if location == abbessLocation
        then do
          connectedLocations <- selectList $ accessibleFrom location
          player <- getPlayer iid
          push
            $ chooseOrRunOne player
            $ targetLabels connectedLocations (only . Move . move attrs iid)
        else push $ move attrs iid abbessLocation
      pure a
    _ -> AbbessAllegriaDiBiase <$> runMessage msg attrs
