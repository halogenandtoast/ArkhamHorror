module Arkham.Homebrew.DarkMatter.Assets.StarfallContacts (
  arNO,
  directorCixin,
  miGoCollector,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Card (toCardCode)
import Arkham.Card.CardDef (CardDef, toCardDef)
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Traits (pattern Starship)
import Arkham.Matcher
import Arkham.Placement

{- | Starfall's three contacts share a shape: each attaches to one location, and
each has an objective that swaps it out for a set-aside objective story asset.

* Ar-NO — attach to Mount Sinai; "If The Cassilda is attached to Mount Sinai" →
  Project Origami.
* Director Cixin — attach to Hope; "If Martian Crab is in the victory display" →
  Last Hope.
* Mi-Go Collector — attach to Threshold of Yuggoth; "If there are no clues on
  Moonbase Laboratory" → Repairing the Threshold.
-}
newtype StarfallContact = StarfallContact AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mkContact :: CardDef -> AssetCard StarfallContact
mkContact = asset StarfallContact

arNO :: AssetCard StarfallContact
arNO = mkContact Cards.arNO

directorCixin :: AssetCard StarfallContact
directorCixin = mkContact Cards.directorCixin

miGoCollector :: AssetCard StarfallContact
miGoCollector = mkContact Cards.miGoCollector

contactIs :: AssetAttrs -> CardDef -> Bool
contactIs a def = toCardCode (toCardDef a) == toCardCode def

-- | The location this contact attaches to.
homeOf :: AssetAttrs -> CardDef
homeOf a
  | contactIs a Cards.directorCixin = Locations.hope
  | contactIs a Cards.miGoCollector = Locations.thresholdOfYuggoth
  | otherwise = Locations.mountSinai

-- | The objective story asset it is replaced by.
unlocks :: AssetAttrs -> CardDef
unlocks a
  | contactIs a Cards.directorCixin = Cards.lastHope
  | contactIs a Cards.miGoCollector = Cards.repairingTheThreshold
  | otherwise = Cards.projectOrigami

-- | The printed objective condition, as a criterion so it gates the ability.
objectiveMet :: AssetAttrs -> Criterion
objectiveMet a
  | contactIs a Cards.directorCixin = InVictoryDisplay (cardIs Enemies.martianCrab) (atLeast 1)
  | contactIs a Cards.miGoCollector =
      exists $ locationIs Locations.moonbaseLaboratory <> LocationWithoutClues
  | otherwise =
      -- The Cassilda prints "connected to attached location and vice versa", so
      -- connection is the observable form of its attachment to Mount Sinai.
      exists
        $ LocationWithTrait Starship
        <> locationIs Locations.theCassilda
        <> connectedTo (locationIs Locations.mountSinai)

instance HasAbilities StarfallContact where
  getAbilities (StarfallContact a) =
    [restricted a 1 (objectiveMet a) $ Objective $ forced AnyWindow]

instance RunMessage StarfallContact where
  runMessage msg a@(StarfallContact attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectOne (locationIs $ homeOf attrs) >>= traverse_ \lid ->
        push $ PlaceAsset attrs.id (AttachedToLocation lid)
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ RemoveFromGame (toTarget attrs)
      card <- getSetAsideCard (unlocks attrs)
      createAssetAt_ card NextToAct
      pure a
    _ -> StarfallContact <$> liftRunMessage msg attrs
