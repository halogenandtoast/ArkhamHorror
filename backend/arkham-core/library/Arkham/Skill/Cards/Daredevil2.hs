module Arkham.Skill.Cards.Daredevil2 (
  daredevil2,
  Daredevil2 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck
import Arkham.Matcher
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Daredevil2 = Daredevil2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

daredevil2 :: SkillCard Daredevil2
daredevil2 = skill Daredevil2 Cards.daredevil2

instance RunMessage Daredevil2 where
  runMessage msg s@(Daredevil2 attrs) = case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      push
        $ RevealUntilFirst iid (toSource attrs) (InvestigatorDeck iid)
        $ CommittableCard iid (basic $ #rogue <> #skill)
      pure s
    RequestedPlayerCard iid (isSource attrs -> True) mcard (map toCard -> rest) -> do
      player <- getPlayer iid
      pushAll $ case mcard of
        Nothing ->
          [ FocusCards rest
          , chooseOne
              player
              [ Label
                  "No cards found"
                  [UnfocusCards, ShuffleCardsIntoDeck (InvestigatorDeck iid) rest]
              ]
          ]
        Just (toCard -> c) ->
          [ FocusCards (rest <> [c])
          , chooseOne
              player
              [ targetLabel
                  (toCardId c)
                  [ UnfocusCards
                  , CommitCard iid c
                  , ShuffleCardsIntoDeck (InvestigatorDeck iid) rest
                  ]
              ]
          ]
      pure s
    _ -> Daredevil2 <$> runMessage msg attrs
