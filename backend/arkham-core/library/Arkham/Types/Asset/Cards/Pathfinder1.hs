module Arkham.Types.Asset.Cards.Pathfinder1
  ( pathfinder1
  , Pathfinder1(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.WindowMatcher

newtype Pathfinder1 = Pathfinder1 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pathfinder1 :: AssetCard Pathfinder1
pathfinder1 = asset Pathfinder1 Cards.pathfinder1

ability :: AssetAttrs -> Ability
ability attrs = (assetAbility
                  attrs
                  1
                  (FreeAbility (Just $ DuringTurn You)
                  $ ExhaustCost (toTarget attrs)
                  )
                )
  { abilityRestrictions = Just InvestigatorNotEngaged
  }

instance HasAbilities Pathfinder1 where
  getAbilities (Pathfinder1 attrs) = [ability attrs]

instance HasModifiersFor env Pathfinder1

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasSet AccessibleLocationId env LocationId
  , HasId LocationId env InvestigatorId
  , HasSet InvestigatorId env ()
  )
  => RunMessage env Pathfinder1 where
  runMessage msg a@(Pathfinder1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      accessibleLocationIds <-
        map unAccessibleLocationId <$> (getSetList =<< getId @LocationId iid)
      a <$ push
        (chooseOne
          iid
          [ MoveAction iid lid Free False | lid <- accessibleLocationIds ]
        )
    _ -> Pathfinder1 <$> runMessage msg attrs
