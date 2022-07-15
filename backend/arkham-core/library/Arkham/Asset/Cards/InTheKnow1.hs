module Arkham.Asset.Cards.InTheKnow1
  ( inTheKnow1
  , InTheKnow1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability hiding (UsedAbility)
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Matcher
import Arkham.Projection

newtype InTheKnow1 = InTheKnow1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheKnow1 :: AssetCard InTheKnow1
inTheKnow1 = asset InTheKnow1 Cards.inTheKnow1

instance HasAbilities InTheKnow1 where
  getAbilities (InTheKnow1 attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ActionAbility (Just Action.Investigate)
        $ ActionCost 1
        <> UseCost (AssetWithId $ toId attrs) Secret 1
    ]

instance RunMessage InTheKnow1 where
  runMessage msg a@(InTheKnow1 attrs) = case msg of
    UseCardAbility iid source windows' 1 _ | isSource attrs source -> do
      investigatorLocation <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        iid
      locations <- selectList $ RevealedLocation <> InvestigatableLocation
      locationsWithInvestigate <- concat <$> for
        locations
        \lid -> do
          investigateActions <-
            selectList
            $ AbilityOnLocation (LocationWithId lid)
            <> AbilityIsAction Action.Investigate
          pure $ map (lid, ) investigateActions
      push $ chooseOne
        iid
        [ targetLabel
            location
            [ SetLocationAsIf iid location
            , UsedAbility iid investigate windows'
            , SetLocationAsIf iid investigatorLocation
            ]
        | (location, investigate) <- locationsWithInvestigate
        ]
      pure a
    _ -> InTheKnow1 <$> runMessage msg attrs
