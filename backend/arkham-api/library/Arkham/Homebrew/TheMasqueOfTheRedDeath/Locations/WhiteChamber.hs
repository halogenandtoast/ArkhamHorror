module Arkham.Homebrew.TheMasqueOfTheRedDeath.Locations.WhiteChamber (whiteChamber) where

import Arkham.Ability
import Arkham.ChaosToken (pattern NegativeModifier)
import Arkham.ChaosToken.Types (ChaosTokenValue (ChaosTokenValue))
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (getSkillTestSource, withSkillTest)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Helpers.Window (discoverSource, getDiscover)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (
  chamberToll,
  describedSkullEffect,
  tollDoomMayAdvanceAgenda,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype WhiteChamber = WhiteChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whiteChamber :: LocationCard WhiteChamber
whiteChamber =
  locationWith WhiteChamber Cards.whiteChamber 3 (PerPlayer 2)
    $ costToEnterUnrevealedL
    .~ chamberToll

instance HasAbilities WhiteChamber where
  getAbilities (WhiteChamber a) =
    extendRevealed
      a
      -- "[skull]: 0. -2 instead if this test is printed on an asset you control."
      [ describedSkullEffect 0 "0. -2 instead if this test is printed on an asset you control." a 1
      , -- "[reaction] When you would discover 1 or more clues from White Chamber via
        -- an ability printed on an asset: Discover 1 additional clue. (Group limit
        -- once per round.)"
        groupLimit PerRound
          $ restricted a 2 Here
          $ freeReaction
          $ WouldDiscoverClues #when You (be a) (atLeast 1) (SourceIsAsset AnyAsset)
      ]

instance RunMessage WhiteChamber where
  runMessage msg l@(WhiteChamber attrs) = runQueueT do
    tollDoomMayAdvanceAgenda attrs msg
    case msg of
      UseThisAbility iid (isSource attrs -> True) 1 -> do
        onAsset <-
          maybe (pure False) (`sourceMatches` SourceIsAsset (assetControlledBy iid))
            =<< getSkillTestSource
        when onAsset do
          withSkillTest \sid ->
            skillTestModifier sid (attrs.ability 1) sid
              $ AddChaosTokenValue (ChaosTokenValue #skull (NegativeModifier 2))
        pure l
      UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
        for_ (discoverSource ws) \source -> do
          whenM (sourceMatches source $ SourceIsAsset AnyAsset) do
            roundModifier (attrs.ability 2) (DiscoverTarget $ getDiscover ws) (DiscoveredClues 1)
        pure l
      _ -> WhiteChamber <$> liftRunMessage msg attrs
