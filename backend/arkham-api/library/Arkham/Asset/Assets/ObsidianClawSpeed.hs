module Arkham.Asset.Assets.ObsidianClawSpeed (obsidianClaw) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Scenarios.ObsidianCanyons.Helpers (gridLocationsWithin)

newtype ObsidianClawSpeed = ObsidianClawSpeed AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianClaw :: AssetCard ObsidianClawSpeed
obsidianClaw = asset ObsidianClawSpeed Cards.obsidianClaw

instance HasModifiersFor ObsidianClawSpeed where
  getModifiersFor (ObsidianClawSpeed a) = artifactModifiers a

instance HasAbilities ObsidianClawSpeed where
  getAbilities (ObsidianClawSpeed a) =
    [ -- Only readable once every glyph printed on the card has been translated.
      restricted a 1 (ControlsThis <> DuringTurn You <> glyphsAllKnown "APMEBC")
        $ FastAbility
        $ exhaust a
    , playerLimit PerRound $ controlled_ a 2 $ FastAbility Free
    , artifactAbility a 3
    ]

instance RunMessage ObsidianClawSpeed where
  runMessage msg a@(ObsidianClawSpeed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Choose a location up to 2 connections away: Move to the chosen location."
      -- Obsidian Canyons measures that on its grid; open sky is a location there
      -- but cannot be entered, so it drops out of the candidates on its own.
      locations <- maybe (pure []) (gridLocationsWithin 2) =<< selectOne (locationWithInvestigator iid)
      canEnter <- filterM (<=~> canEnterLocation iid) locations
      chooseTargetM iid canEnter $ moveTo (attrs.ability 1) iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      handOffArtifact iid attrs
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.obsidianClawPower
      pure a
    _ -> ObsidianClawSpeed <$> liftRunMessage msg attrs
