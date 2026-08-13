module Arkham.Homebrew.DarkMatter.Assets.MiGoCollector (miGoCollector) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Placement

{- | One of Starfall's three contacts: attaches to the Threshold of Yuggoth, and
its objective ("If there are no clues on Moonbase Laboratory") swaps it out for
the set-aside Repairing the Threshold objective.
-}
newtype MiGoCollector = MiGoCollector AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoCollector :: AssetCard MiGoCollector
miGoCollector = asset MiGoCollector Cards.miGoCollector

instance HasAbilities MiGoCollector where
  getAbilities (MiGoCollector a) =
    [ restricted a 1 (exists $ locationIs Locations.moonbaseLaboratory <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage MiGoCollector where
  runMessage msg a@(MiGoCollector attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectOne (locationIs Locations.thresholdOfYuggoth)
        >>= traverse_ (push . PlaceAsset attrs.id . AttachedToLocation)
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ RemoveFromGame (toTarget attrs)
      card <- getSetAsideCard Cards.repairingTheThreshold
      createAssetAt_ card NextToAct
      pure a
    _ -> MiGoCollector <$> liftRunMessage msg attrs
