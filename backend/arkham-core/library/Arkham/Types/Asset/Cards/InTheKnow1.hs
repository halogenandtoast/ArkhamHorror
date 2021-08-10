module Arkham.Types.Asset.Cards.InTheKnow1
  ( inTheKnow1
  , InTheKnow1(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype InTheKnow1 = InTheKnow1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheKnow1 :: AssetCard InTheKnow1
inTheKnow1 = asset InTheKnow1 Cards.inTheKnow1

instance HasActions InTheKnow1 where
  getActions (InTheKnow1 attrs) =
    [ restrictedAbility attrs 1 OwnsThis
        $ ActionAbility (Just Action.Investigate)
        $ ActionCost 1
        <> UseCost (toId attrs) Secret 1
    ]

instance HasModifiersFor env InTheKnow1

-- investigate is 101
investigateAction :: [Ability] -> Maybe Ability
investigateAction = find ((== 101) . abilityIndex)

instance
  ( Query LocationMatcher env
  , HasActions env LocationId
  , HasId LocationId env InvestigatorId
  , HasQueue env
  , HasModifiersFor env ()
  )
  => RunMessage env InTheKnow1 where
  runMessage msg a@(InTheKnow1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      investigatorLocation <- getId @LocationId iid
      locations <- selectList $ RevealedLocation <> InvestigatableLocation
      locationsWithActions <- traverse
        (traverseToSnd $ getActions iid NonFast)
        locations
      let
        locationsWithInvestigateActions = mapMaybe
          (\(lid, actions) -> (lid, ) <$> investigateAction actions)
          locationsWithActions
      a <$ push
        (chooseOne
          iid
          [ TargetLabel
              (LocationTarget location)
              [ SetLocationAsIf iid location
              , UseAbility iid investigate
              , SetLocationAsIf iid investigatorLocation
              ]
          | (location, investigate) <- locationsWithInvestigateActions
          ]
        )
    _ -> InTheKnow1 <$> runMessage msg attrs
