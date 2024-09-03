module Arkham.Skill.Cards.Resourceful (
  resourceful,
  Resourceful (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Resourceful = Resourceful SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

resourceful :: SkillCard Resourceful
resourceful = skill Resourceful Cards.resourceful

instance RunMessage Resourceful where
  runMessage msg s@(Resourceful attrs) = case msg of
    PassedSkillTest _ _ _ target _ _ | isTarget attrs target -> do
      targets <-
        select
          ( InDiscardOf (InvestigatorWithId $ skillOwner attrs)
              <> BasicCardMatch
                (CardWithClass Survivor <> NotCard (CardWithTitle "Resourceful"))
          )
      player <- getPlayer (skillOwner attrs)
      s
        <$ when
          (notNull targets)
          ( push
              $ chooseOne
                player
                [ TargetLabel
                  (CardIdTarget $ toCardId card)
                  [ RemoveFromDiscard (skillOwner attrs) (toCardId card)
                  , addToHand (skillOwner attrs) card
                  ]
                | card <- targets
                ]
          )
    _ -> Resourceful <$> runMessage msg attrs
