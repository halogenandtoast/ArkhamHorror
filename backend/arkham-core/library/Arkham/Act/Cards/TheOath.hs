module Arkham.Act.Cards.TheOath (
  TheOath (..),
  theOath,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Trait

newtype TheOath = TheOath ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theOath :: ActCard TheOath
theOath =
  act
    (3, A)
    TheOath
    Cards.theOath
    (Just $ GroupClueCost (PerPlayer 3) (locationIs Locations.hiddenLibrary))

instance HasModifiersFor TheOath where
  getModifiersFor (LocationTarget _) (TheOath attrs) = do
    pure $
      toModifiers
        attrs
        [ ConnectedToWhen
            (LocationWithTrait Passageway)
            (LocationWithTrait Passageway)
        ]
  getModifiersFor _ _ = pure []

instance RunMessage TheOath where
  runMessage msg a@(TheOath attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      a
        <$ push
          ( chooseOne
              leadInvestigatorId
              [ Label
                  "This is an important discovery! We should take it. (-> R1)"
                  [ScenarioResolution $ Resolution 1]
              , Label
                  "It's just a silly trinket, and it would be wrong to steal from the Historical Society. Leave it behind (-> R2)"
                  [ScenarioResolution $ Resolution 2]
              ]
          )
    _ -> TheOath <$> runMessage msg attrs
