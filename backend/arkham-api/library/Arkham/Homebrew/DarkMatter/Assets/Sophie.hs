module Arkham.Homebrew.DarkMatter.Assets.Sophie (sophie) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scanAction, scanTopOfScanningDeck)
import Arkham.Matcher

newtype Sophie = Sophie AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sophie :: AssetCard Sophie
sophie = asset Sophie Cards.sophie

{- | "[action] Spend 1[per_investigator] clues: Scan. Draw the top card of the
scanning deck as if you were at any location. (Group limit once per game.)"
-}
instance HasAbilities Sophie where
  getAbilities (Sophie a) =
    [ groupLimit PerGame
        $ controlled a 1 ControlsThis
        $ scanAction (GroupClueCost (PerPlayer 1) Anywhere)
    ]

instance RunMessage Sophie where
  runMessage msg a@(Sophie attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scanTopOfScanningDeck iid (attrs.ability 1)
      pure a
    _ -> Sophie <$> liftRunMessage msg attrs
