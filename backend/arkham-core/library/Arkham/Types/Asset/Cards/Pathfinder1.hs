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
import Arkham.Types.Restriction

newtype Pathfinder1 = Pathfinder1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pathfinder1 :: AssetCard Pathfinder1
pathfinder1 = asset Pathfinder1 Cards.pathfinder1

instance HasActions Pathfinder1 where
  getActions (Pathfinder1 attrs) =
    [ restrictedAbility
        attrs
        1
        (OwnsThis <> InvestigatorExists (You <> UnengagedInvestigator))
        (FastAbility $ ExhaustCost (toTarget attrs))
    ]

instance HasModifiersFor env Pathfinder1

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasSet AccessibleLocationId env LocationId
  , HasId LocationId env InvestigatorId
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
