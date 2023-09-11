module Arkham.Event.Cards.SureGamble3 where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message hiding (RevealChaosToken)
import Arkham.Window

newtype SureGamble3 = SureGamble3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sureGamble3 :: EventCard SureGamble3
sureGamble3 = event SureGamble3 Cards.sureGamble3

toRevealedToken :: [Window] -> ChaosToken
toRevealedToken [] = error "invalid"
toRevealedToken ((windowType -> RevealChaosToken _ token) : _) = token
toRevealedToken (_ : rest) = toRevealedToken rest

instance RunMessage SureGamble3 where
  runMessage msg e@(SureGamble3 attrs) = case msg of
    InvestigatorPlayEvent _ eid _ (toRevealedToken -> token) _ | eid == toId attrs -> do
      pushAll
        [ CreateEffect "01088" Nothing (toSource attrs) (ChaosTokenTarget token)
        ]
      pure e
    _ -> SureGamble3 <$> runMessage msg attrs
