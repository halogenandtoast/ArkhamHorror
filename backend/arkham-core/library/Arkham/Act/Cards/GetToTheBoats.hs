module Arkham.Act.Cards.GetToTheBoats
  ( GetToTheBoats(..)
  , getToTheBoats
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Criteria
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype GetToTheBoats = GetToTheBoats ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getToTheBoats :: ActCard GetToTheBoats
getToTheBoats = act (2, A) GetToTheBoats Cards.getToTheBoats Nothing

instance HasAbilities GetToTheBoats where
  getAbilities (GetToTheBoats x) | onSide A x =
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
  getAbilities _ = []

instance RunMessage GetToTheBoats where
  runMessage msg a@(GetToTheBoats attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      gondola <- genEncounterCard Locations.gondola
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ InvestigatorDrewEncounterCard leadInvestigatorId gondola
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      targets <- selectListMap
        AssetTarget
        (AssetWithTitle "Masked Carnevale-Goer")
      unless (null targets) $ pushAll
        [ chooseOne
            iid
            [ TargetLabel target [Flip iid source target] | target <- targets ]
        ]
      pure a
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    _ -> GetToTheBoats <$> runMessage msg attrs
