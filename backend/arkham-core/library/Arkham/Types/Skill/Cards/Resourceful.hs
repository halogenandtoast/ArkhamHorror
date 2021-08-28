module Arkham.Types.Skill.Cards.Resourceful
  ( resourceful
  , Resourceful(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Target

newtype Resourceful = Resourceful SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

resourceful :: SkillCard Resourceful
resourceful = skill Resourceful Cards.resourceful

instance SkillRunner env => RunMessage env Resourceful where
  runMessage msg s@(Resourceful attrs) = case msg of
    PassedSkillTest _ _ _ target _ _ | isTarget attrs target -> do
      targets <- selectList
        (InDiscardOf (InvestigatorWithId $ skillOwner attrs) <> BasicCardMatch
          (CardWithClass Survivor <> NotCard (CardWithTitle "Resourceful"))
        )
      s <$ when
        (notNull targets)
        (push $ chooseOne
          (skillOwner attrs)
          [ TargetLabel
              (CardIdTarget $ toCardId card)
              [ RemoveFromDiscard (skillOwner attrs) (toCardId card)
              , AddToHand (skillOwner attrs) card
              ]
          | card <- targets
          ]
        )
    _ -> Resourceful <$> runMessage msg attrs
