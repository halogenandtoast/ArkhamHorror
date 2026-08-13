module Arkham.Homebrew.DarkMatter.Assets.ArNO (arNO) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Traits (pattern Starship)
import Arkham.Matcher
import Arkham.Placement

{- | One of Starfall's three contacts: attaches to Mount Sinai, and its objective
("If The Cassilda is attached to Mount Sinai") swaps it out for the set-aside
Project Origami objective.
-}
newtype ArNO = ArNO AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arNO :: AssetCard ArNO
arNO = asset ArNO Cards.arNO

instance HasAbilities ArNO where
  getAbilities (ArNO a) =
    [ restricted a 1 objectiveMet $ Objective $ forced AnyWindow
    ]
   where
    -- The Cassilda prints "connected to attached location and vice versa", so
    -- connection is the observable form of its attachment to Mount Sinai.
    objectiveMet =
      exists
        $ LocationWithTrait Starship
        <> locationIs Locations.theCassilda
        <> connectedTo (locationIs Locations.mountSinai)

instance RunMessage ArNO where
  runMessage msg a@(ArNO attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectOne (locationIs Locations.mountSinai)
        >>= traverse_ (push . PlaceAsset attrs.id . AttachedToLocation)
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ RemoveFromGame (toTarget attrs)
      card <- getSetAsideCard Cards.projectOrigami
      createAssetAt_ card NextToAct
      pure a
    _ -> ArNO <$> liftRunMessage msg attrs
