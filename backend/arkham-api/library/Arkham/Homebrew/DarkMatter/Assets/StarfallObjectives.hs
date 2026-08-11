module Arkham.Homebrew.DarkMatter.Assets.StarfallObjectives (
  lastHope,
  projectOrigami,
  repairingTheThreshold,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Card (toCardCode)
import Arkham.Card.CardDef (CardDef, toCardDef)
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Placement

{- | Starfall's three escape objectives share a shape: an investigator at a
particular location must control a particular item, which is then removed from
the game and paid for with 2[per_investigator] clues as a group.

* Project Origami — Mount Sinai + Universal Archives
* Last Hope — Hope + Shielding Device
* Repairing the Threshold — Threshold of Yuggoth + Stasis Cube
-}
newtype StarfallObjective = StarfallObjective AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mkObjective :: CardDef -> AssetCard StarfallObjective
mkObjective = asset StarfallObjective

projectOrigami :: AssetCard StarfallObjective
projectOrigami = mkObjective Cards.projectOrigami

lastHope :: AssetCard StarfallObjective
lastHope = mkObjective Cards.lastHope

repairingTheThreshold :: AssetCard StarfallObjective
repairingTheThreshold = mkObjective Cards.repairingTheThreshold

objectiveIs :: AssetAttrs -> CardDef -> Bool
objectiveIs a def = toCardCode (toCardDef a) == toCardCode def

-- | The location the bearer must be at.
site :: AssetAttrs -> CardDef
site a
  | objectiveIs a Cards.lastHope = Locations.hope
  | objectiveIs a Cards.repairingTheThreshold = Locations.thresholdOfYuggoth
  | otherwise = Locations.mountSinai

-- | The item that must be controlled there, and is removed from the game.
tribute :: AssetAttrs -> CardDef
tribute a
  | objectiveIs a Cards.lastHope = Cards.shieldingDevice
  | objectiveIs a Cards.repairingTheThreshold = Cards.stasisCube
  | otherwise = Cards.universalArchives

instance HasAbilities StarfallObjective where
  getAbilities (StarfallObjective a) =
    [ restricted
        a
        1
        (exists $ assetIs (tribute a) <> AssetControlledBy (InvestigatorAt $ locationIs $ site a))
        $ Objective
        $ FastAbility
        $ GroupClueCost (PerPlayer 2) Anywhere
    ]

instance RunMessage StarfallObjective where
  runMessage msg a@(StarfallObjective attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      push $ PlaceAsset attrs.id NextToAct
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (assetIs $ tribute attrs) >>= traverse_ \item ->
        push $ RemoveFromGame (toTarget item)
      addToVictory iid attrs
      pure a
    _ -> StarfallObjective <$> liftRunMessage msg attrs
