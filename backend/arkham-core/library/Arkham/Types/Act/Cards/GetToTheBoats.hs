module Arkham.Types.Act.Cards.GetToTheBoats
  ( GetToTheBoats(..)
  , getToTheBoats
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Phase
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype GetToTheBoats = GetToTheBoats ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getToTheBoats :: ActCard GetToTheBoats
getToTheBoats = act (2, A) GetToTheBoats Cards.getToTheBoats Nothing

instance HasAbilities GetToTheBoats where
  getAbilities (GetToTheBoats x) =
    [ mkAbility x 1 $ ForcedAbility $ PhaseBegins Timing.After $ PhaseIs
      MythosPhase
    , restrictedAbility
      x
      2
      (EachUndefeatedInvestigator $ InvestigatorAt $ LocationWithTitle
        "Canal-side"
      )
    $ Objective
    $ ForcedAbility AnyWindow
    ]

instance ActRunner env => RunMessage env GetToTheBoats where
  runMessage msg a@(GetToTheBoats attrs) = case msg of
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      gondola <- genEncounterCard Locations.gondola
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ InvestigatorDrewEncounterCard leadInvestigatorId gondola
        , NextAct (toId attrs) "82007"
        ]
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      maskedCarnevaleGoers <- selectListMap
        AssetTarget
        (AssetWithTitle "Masked Carnevale-Goer")
      case maskedCarnevaleGoers of
        [] -> pure a
        xs -> a <$ pushAll [chooseOne iid [ Flip source x | x <- xs ]]
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) source)
    _ -> GetToTheBoats <$> runMessage msg attrs
