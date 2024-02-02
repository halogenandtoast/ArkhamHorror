module Arkham.Event.Cards.ParallelFates (
  parallelFates,
  ParallelFates (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.RequestedChaosTokenStrategy

newtype Metadata = Metadata {drawnCards :: [EncounterCard]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

newtype ParallelFates = ParallelFates (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

parallelFates :: EventCard ParallelFates
parallelFates =
  event (ParallelFates . (`with` Metadata [])) Cards.parallelFates

instance RunMessage ParallelFates where
  runMessage msg e@(ParallelFates (attrs `With` meta)) = case msg of
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs -> do
      push (DrawEncounterCards (toTarget attrs) 4)
      pure e
    RequestedEncounterCards target cards | isTarget attrs target -> do
      pushAll
        [ FocusCards (map EncounterCard cards)
        , RequestChaosTokens
            (toSource attrs)
            (Just $ eventOwner attrs)
            (Reveal 1)
            SetAside
        , UnfocusCards
        ]
      pure $ ParallelFates (attrs `With` Metadata cards)
    RequestedChaosTokens (isSource attrs -> True) (Just iid) (map chaosTokenFace -> tokens) -> do
      player <- getPlayer iid
      push $ ResetChaosTokens (toSource attrs)
      push
        $ if any (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) tokens
          then
            chooseOne
              player
              [ Label
                  "Shuffle back in"
                  [ ShuffleCardsIntoDeck
                      EncounterDeck
                      (map EncounterCard $ drawnCards meta)
                  ]
              ]
          else
            chooseOneAtATime
              player
              [ targetLabel
                (toCardId c)
                [PutCardOnTopOfDeck iid EncounterDeck (EncounterCard c)]
              | c <- drawnCards meta
              ]
      pure e
    _ -> ParallelFates . (`with` meta) <$> runMessage msg attrs
