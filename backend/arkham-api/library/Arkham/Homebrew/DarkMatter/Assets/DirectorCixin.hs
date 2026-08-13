module Arkham.Homebrew.DarkMatter.Assets.DirectorCixin (directorCixin) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Placement

{- | One of Starfall's three contacts: attaches to Hope, and its objective ("If
Martian Crab is in the victory display") swaps it out for the set-aside Last Hope
objective.
-}
newtype DirectorCixin = DirectorCixin AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

directorCixin :: AssetCard DirectorCixin
directorCixin = asset DirectorCixin Cards.directorCixin

instance HasAbilities DirectorCixin where
  getAbilities (DirectorCixin a) =
    [ restricted a 1 (InVictoryDisplay (cardIs Enemies.martianCrab) (atLeast 1))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage DirectorCixin where
  runMessage msg a@(DirectorCixin attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectOne (locationIs Locations.hope)
        >>= traverse_ (push . PlaceAsset attrs.id . AttachedToLocation)
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ RemoveFromGame (toTarget attrs)
      card <- getSetAsideCard Cards.lastHope
      createAssetAt_ card NextToAct
      pure a
    _ -> DirectorCixin <$> liftRunMessage msg attrs
