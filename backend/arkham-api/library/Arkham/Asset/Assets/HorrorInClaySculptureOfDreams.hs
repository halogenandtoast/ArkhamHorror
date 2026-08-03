module Arkham.Asset.Assets.HorrorInClaySculptureOfDreams (horrorInClay) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (StarSpawn))

newtype HorrorInClaySculptureOfDreams = HorrorInClaySculptureOfDreams AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horrorInClay :: AssetCard HorrorInClaySculptureOfDreams
horrorInClay = asset HorrorInClaySculptureOfDreams Cards.horrorInClay

{- | "Each [[Star Spawn]] enemy loses patrol and its printed <b>Prey</b> instructions
and gains hunter and '<b>Prey</b> - You.'" @ForcePrey@ both discards the printed prey
instruction and installs the replacement, aimed at this asset's controller.
-}
instance HasModifiersFor HorrorInClaySculptureOfDreams where
  getModifiersFor (HorrorInClaySculptureOfDreams a) = do
    artifactModifiers a
    for_ a.controller \iid ->
      modifySelect
        a
        (EnemyWithTrait StarSpawn)
        [LosePatrol, AddKeyword Keyword.Hunter, ForcePrey (Prey $ InvestigatorWithId iid)]

instance HasAbilities HorrorInClaySculptureOfDreams where
  getAbilities (HorrorInClaySculptureOfDreams a) =
    -- The glyphs sit above the [fast] ability, so it stays blank until every one of
    -- them has been translated.
    [ controlled a 1 (glyphsAllKnown "UXOODFZ") $ FastAbility (exhaust a)
    , artifactAbility a 2
    ]

instance RunMessage HorrorInClaySculptureOfDreams where
  runMessage msg a@(HorrorInClaySculptureOfDreams attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      starSpawn <- select $ EnemyWithTrait StarSpawn
      campaignI18n $ chooseOneM iid do
        -- Blanking the sculpture also takes the hunter/prey grant above with it,
        -- which is the whole point of turning it off for a round.
        labeled' "horrorInClay.blankPrintedText" $ roundModifier (attrs.ability 1) attrs Blank
        when (notNull starSpawn) do
          labeled' "horrorInClay.moveStarSpawn" $ chooseTargetM iid starSpawn \eid -> do
            connecting <- select $ connectedFrom (locationWithEnemy eid) <> LocationCanBeEnteredBy eid
            chooseTargetM iid connecting $ enemyMoveTo (attrs.ability 1) eid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      handOffArtifact iid attrs
      pure a
    _ -> HorrorInClaySculptureOfDreams <$> liftRunMessage msg attrs
