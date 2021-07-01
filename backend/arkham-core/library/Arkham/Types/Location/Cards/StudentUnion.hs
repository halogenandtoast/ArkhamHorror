module Arkham.Types.Location.Cards.StudentUnion
  ( StudentUnion(..)
  , studentUnion
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (studentUnion)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype StudentUnion = StudentUnion LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

studentUnion :: LocationId -> StudentUnion
studentUnion = StudentUnion . baseAttrs
  Cards.studentUnion
  1
  (Static 2)
  Diamond
  [Plus, Equals]

instance HasModifiersFor env StudentUnion where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env StudentUnion where
  getActions iid NonFast (StudentUnion attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ do
      let
        ability =
          mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 2)
      pure
        [ ActivateCardAbilityAction iid ability
        | iid `elem` locationInvestigators
        ]
  getActions iid window (StudentUnion attrs) = getActions iid window attrs

instance (LocationRunner env) => RunMessage env StudentUnion where
  runMessage msg l@(StudentUnion attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      unshiftMessage $ PlaceLocationMatching (LocationWithTitle "Dormitories")
      StudentUnion <$> runMessage msg attrs
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessages
        [ HealDamage (InvestigatorTarget iid) 1
        , HealHorror (InvestigatorTarget iid) 1
        ]
    _ -> StudentUnion <$> runMessage msg attrs
