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
import Arkham.Types.Window

newtype Pathfinder1 = Pathfinder1 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pathfinder1 :: AssetCard Pathfinder1
pathfinder1 = asset Pathfinder1 Cards.pathfinder1

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (FastAbility $ ExhaustCost (toTarget attrs))

instance HasSet EnemyId env InvestigatorId => HasActions env Pathfinder1 where
  getActions iid (DuringTurn You) (Pathfinder1 attrs) = do
    engagedEnemies <- getSet @EnemyId iid
    pure [ ActivateCardAbilityAction iid (ability attrs) | null engagedEnemies ]
  getActions _ _ _ = pure []

instance HasModifiersFor env Pathfinder1 where
  getModifiersFor = noModifiersFor

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
      a <$ unshiftMessage
        (chooseOne
          iid
          [ MoveAction iid lid Free False | lid <- accessibleLocationIds ]
        )
    _ -> Pathfinder1 <$> runMessage msg attrs
