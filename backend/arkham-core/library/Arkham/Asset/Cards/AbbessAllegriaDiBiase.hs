module Arkham.Asset.Cards.AbbessAllegriaDiBiase
  ( abbessAllegriaDiBiase
  , AbbessAllegriaDiBiase(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Types
import Arkham.Matcher hiding ( MoveAction )
import Arkham.Projection

newtype AbbessAllegriaDiBiase = AbbessAllegriaDiBiase AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abbessAllegriaDiBiase :: AssetCard AbbessAllegriaDiBiase
abbessAllegriaDiBiase =
  ally AbbessAllegriaDiBiase Cards.abbessAllegriaDiBiase (2, 2)

instance HasAbilities AbbessAllegriaDiBiase where
  getAbilities (AbbessAllegriaDiBiase attrs) =
    [ restrictedAbility
        attrs
        1
        (AnyCriterion
          [ (OnSameLocation <> LocationExists AccessibleLocation)
          , LocationExists $ LocationWithAsset $ AssetWithId $ toId attrs
          ]
        )
        (FastAbility $ ExhaustCost (toTarget attrs))
    ]

instance RunMessage AbbessAllegriaDiBiase where
  runMessage msg a@(AbbessAllegriaDiBiase attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      mLocationId <- field InvestigatorLocation iid
      case mLocationId of
        Nothing -> error "impossible"
        Just locationId -> do
          abbessLocationId <- fromJustNote "locationIsRequired"
            <$> field AssetLocation (toId attrs)
          a <$ if locationId == abbessLocationId
            then do
              connectedLocationIds <-
                selectList $ AccessibleFrom $ LocationWithId locationId
              push
                (chooseOrRunOne
                  iid
                  [ TargetLabel
                      (LocationTarget connectedLocationId)
                      [MoveAction iid connectedLocationId Free False]
                  | connectedLocationId <- connectedLocationIds
                  ]
                )
            else push (MoveAction iid abbessLocationId Free False)
    _ -> AbbessAllegriaDiBiase <$> runMessage msg attrs
