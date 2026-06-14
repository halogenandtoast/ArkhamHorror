module Arkham.Asset.Assets.HorrorInClaySculptureOfDreams (horrorInClay) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.TheDoomOfArkhamPartI.Helpers
import Arkham.Trait (Trait (StarSpawn))

newtype HorrorInClaySculptureOfDreams = HorrorInClaySculptureOfDreams AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horrorInClay :: AssetCard HorrorInClaySculptureOfDreams
horrorInClay = asset HorrorInClaySculptureOfDreams Cards.horrorInClay

-- Each [[Star Spawn]] enemy loses patrol and its printed Prey instructions and
-- gains hunter and "Prey - You." The ForcePrey override both removes the
-- printed Prey and installs "Prey - You" (relative to this asset's controller).
instance HasModifiersFor HorrorInClaySculptureOfDreams where
  getModifiersFor (HorrorInClaySculptureOfDreams a) = for_ a.controller \iid ->
    modifySelect
      a
      (EnemyWithTrait StarSpawn)
      [LosePatrol, AddKeyword Keyword.Hunter, ForcePrey (Prey $ InvestigatorWithId iid)]

instance HasAbilities HorrorInClaySculptureOfDreams where
  getAbilities (HorrorInClaySculptureOfDreams a) =
    [restricted a 1 ControlsThis $ FastAbility (exhaust a)]

instance RunMessage HorrorInClaySculptureOfDreams where
  runMessage msg a@(HorrorInClaySculptureOfDreams attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      starSpawn <- select $ EnemyWithTrait StarSpawn
      chooseOneM iid $ scenarioI18n do
        labeled' "blankPrintedText" $ roundModifier (attrs.ability 1) attrs Blank
        when (notNull starSpawn) do
          labeled' "moveStarSpawn" $ chooseTargetM iid starSpawn \eid -> do
            connecting <- select $ connectedFrom (locationWithEnemy eid) <> LocationCanBeEnteredBy eid
            chooseTargetM iid connecting $ enemyMoveTo (attrs.ability 1) eid
      pure a
    _ -> HorrorInClaySculptureOfDreams <$> liftRunMessage msg attrs
