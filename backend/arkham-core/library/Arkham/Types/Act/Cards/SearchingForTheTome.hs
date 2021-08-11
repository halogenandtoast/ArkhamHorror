module Arkham.Types.Act.Cards.SearchingForTheTome
  ( SearchingForTheTome(..)
  , searchingForTheTome
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Restriction

newtype SearchingForTheTome = SearchingForTheTome ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

searchingForTheTome :: ActCard SearchingForTheTome
searchingForTheTome =
  act (3, A) SearchingForTheTome Cards.searchingForTheTome Nothing

instance HasActions SearchingForTheTome where
  getActions (SearchingForTheTome x) =
    restrictedAbility
        x
        1
        (LocationExists
          (LocationWithFullTitle "Exhibit Hall" "Restricted Hall"
          <> LocationWithoutClues
          )
        )
        (Objective $ ForcedAbility AnyWindow)
      : getActions x

instance ActRunner env => RunMessage env SearchingForTheTome where
  runMessage msg a@(SearchingForTheTome attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ push
        (chooseOne
          leadInvestigatorId
          [ Label
            "It's too dangerous to keep around. We have to destroy it. (-> R1)"
            [ScenarioResolution $ Resolution 1]
          , Label
            "It's too valuable to destroy. We have to keep it safe. (-> R2)"
            [ScenarioResolution $ Resolution 2]
          ]
        )
    _ -> SearchingForTheTome <$> runMessage msg attrs
