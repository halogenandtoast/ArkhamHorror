module Arkham.Asset.Assets.ObsidianClawSpeed (obsidianClaw) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.I18n
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype ObsidianClawSpeed = ObsidianClawSpeed AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianClaw :: AssetCard ObsidianClawSpeed
obsidianClaw = asset ObsidianClawSpeed Cards.obsidianClaw

instance HasModifiersFor ObsidianClawSpeed where
  getModifiersFor (ObsidianClawSpeed a) = artifactModifiers a

instance HasAbilities ObsidianClawSpeed where
  getAbilities (ObsidianClawSpeed a) =
    [ cardI18n (withI18nTooltip "obsidianClaw.move")
        $ controlled a 1 (DuringTurn You)
        $ freeTrigger
        $ exhaust a
    , cardI18n (withI18nTooltip "obsidianClaw.flip")
        $ limited (MaxPer Cards.obsidianClaw PerRound 1)
        $ controlled a 2 (glyphsAllKnown "APMEBC")
        $ FastAbility Free
    , artifactAbility a 3
    ]

instance RunMessage ObsidianClawSpeed where
  runMessage msg a@(ObsidianClawSpeed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <-
        select
          $ LocationWithDistanceFromAtMost
            2
            (locationWithInvestigator iid)
            (not_ (locationWithInvestigator iid) <> CanEnterLocation (InvestigatorWithId iid))
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
