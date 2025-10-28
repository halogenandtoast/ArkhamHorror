module Arkham.Asset.Assets.AbbessAllegriaDiBiase (abbessAllegriaDiBiase) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection

newtype AbbessAllegriaDiBiase = AbbessAllegriaDiBiase AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abbessAllegriaDiBiase :: AssetCard AbbessAllegriaDiBiase
abbessAllegriaDiBiase = ally AbbessAllegriaDiBiase Cards.abbessAllegriaDiBiase (2, 2)

instance HasAbilities AbbessAllegriaDiBiase where
  getAbilities (AbbessAllegriaDiBiase a) =
    [ fastAbility a 1 (exhaust a)
        $ oneOf
          [ OnSameLocation <> exists AccessibleLocation
          , exists $ YourLocation <> ConnectedTo ForMovement (locationWithAsset a)
          ]
    ]

instance RunMessage AbbessAllegriaDiBiase where
  runMessage msg a@(AbbessAllegriaDiBiase attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      location <- getJustLocation iid
      abbessLocation <- fieldJust AssetLocation attrs
      if location == abbessLocation
        then do
          connectedLocations <- select $ accessibleFrom ForMovement location
          chooseOrRunOneM iid $ targets connectedLocations (moveTo attrs iid)
        else moveTo attrs iid abbessLocation
      pure a
    _ -> AbbessAllegriaDiBiase <$> liftRunMessage msg attrs
