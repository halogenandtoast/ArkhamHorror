module Arkham.Event.Cards.GhastlyRevelation (
  ghastlyRevelation,
  GhastlyRevelation (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message qualified as Msg
import Arkham.Projection

newtype GhastlyRevelation = GhastlyRevelation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghastlyRevelation :: EventCard GhastlyRevelation
ghastlyRevelation = event GhastlyRevelation Cards.ghastlyRevelation

instance RunMessage GhastlyRevelation where
  runMessage msg e@(GhastlyRevelation attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ Msg.DiscoverClues iid $ discoverAtYourLocation (toSource attrs) 3
        , ResolveEvent iid eid Nothing []
        ]
      pure e
    ResolveEvent iid eid _ _ | eid == toId attrs -> do
      clues <- field InvestigatorClues iid
      otherInvestigators <- select $ affectsOthers $ notInvestigator iid
      locations <- select Anywhere
      player <- getPlayer iid
      choices1 <- for otherInvestigators $ \iid' -> do
        chooseMsg <-
          chooseAmounts
            player
            "Clues to give"
            (MaxAmountTarget clues)
            [("Clues", (0, clues))]
            (ProxyTarget (toTarget attrs) (InvestigatorTarget iid'))
        pure $ targetLabel iid' [chooseMsg]

      choices2 <- for locations $ \lid -> do
        chooseMsg <-
          chooseAmounts
            player
            "Clues to give"
            (MaxAmountTarget clues)
            [("Clues", (0, clues))]
            (ProxyTarget (toTarget attrs) (LocationTarget lid))
        pure $ targetLabel lid [chooseMsg]

      push
        $ chooseOrRunOne player
        $ [ Label "Give any number of your clues to another investigator" [chooseOrRunOne player choices1]
          | notNull otherInvestigators
          ]
        <> [Label "Place any number of your clues on any location" [chooseOrRunOne player choices2]]
      pure e
    ResolveAmounts iid (getChoiceAmount "Clues" -> n) (ProxyTarget (isTarget attrs -> True) target) ->
      do
        pushAll
          [ InvestigatorSpendClues iid n
          , PlaceClues (toSource attrs) target n
          , SufferTrauma iid 0 1
          , InvestigatorDefeated (toSource attrs) iid
          ]
        pure e
    _ -> GhastlyRevelation <$> runMessage msg attrs
