module Arkham.Asset.Cards.InTheKnow1 (
  inTheKnow1,
  InTheKnow1 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype InTheKnow1 = InTheKnow1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

inTheKnow1 :: AssetCard InTheKnow1
inTheKnow1 = asset InTheKnow1 Cards.inTheKnow1

instance HasAbilities InTheKnow1 where
  getAbilities (InTheKnow1 attrs) =
    [restrictedAbility attrs 1 ControlsThis $ investigateAction $ assetUseCost attrs Secret 1]

instance RunMessage InTheKnow1 where
  runMessage msg a@(InTheKnow1 attrs) = case msg of
    UseCardAbility iid source 1 windows' _ | isSource attrs source -> do
      locations <- selectList $ RevealedLocation <> InvestigatableLocation
      locationsWithInvestigate <- concatForM locations \lid -> do
        investigateActions <-
          selectList $ AbilityOnLocation (LocationWithId lid) <> AbilityIsAction #investigate
        pure $ map (lid,) investigateActions
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ targetLabel location
            $ [ abilityModifier (toAbilitySource attrs 1) iid (AsIfAt location)
              , UseAbility iid ability windows'
              ]
          | (location, ability) <- locationsWithInvestigate
          ]
      pure a
    _ -> InTheKnow1 <$> runMessage msg attrs
