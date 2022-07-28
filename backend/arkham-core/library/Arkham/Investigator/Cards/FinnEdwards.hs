module Arkham.Investigator.Cards.FinnEdwards
  ( finnEdwards
  , FinnEdwards(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Action.Additional
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype FinnEdwards = FinnEdwards InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finnEdwards :: InvestigatorCard FinnEdwards
finnEdwards = investigator
  FinnEdwards
  Cards.finnEdwards
  Stats
    { health = 7
    , sanity = 7
    , willpower = 1
    , intellect = 4
    , combat = 3
    , agility = 4
    }

instance HasAbilities FinnEdwards where
  getAbilities (FinnEdwards _) = []

instance HasTokenValue FinnEdwards where
  getTokenValue iid ElderSign (FinnEdwards attrs) | iid == toId attrs = do
    n <- selectCount ExhaustedEnemy
    pure $ TokenValue ElderSign (PositiveModifier n)
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage FinnEdwards where
  runMessage msg i@(FinnEdwards attrs) = case msg of
    PassedSkillTest iid _ _ (TokenTarget token) _ n
      | iid == toId attrs && n >= 2 -> do
        when (tokenFace token == ElderSign) $ do
          clueCount <- selectAgg
            Sum
            LocationClues
            (locationWithInvestigator iid)
          when (clueCount > 0) $ push $ chooseOne
            iid
            [ Label
              "Discover 1 clue at your location"
              [InvestigatorDiscoverCluesAtTheirLocation iid 1 Nothing]
            , Label "Do not discover a clue" []
            ]
        pure i
    Setup -> FinnEdwards <$> runMessage
      msg
      (attrs
      & additionalActionsL
      %~ (ActionRestrictedAdditionalAction Action.Evade :)
      )
    BeginRound -> FinnEdwards <$> runMessage
      msg
      (attrs
      & additionalActionsL
      %~ (ActionRestrictedAdditionalAction Action.Evade :)
      )
    _ -> FinnEdwards <$> runMessage msg attrs
