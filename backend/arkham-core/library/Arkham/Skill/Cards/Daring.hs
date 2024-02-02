module Arkham.Skill.Cards.Daring (
  daring,
  Daring (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Keyword qualified as Keyword
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

-- Ruling by MJ states the enemy gains to be a lasting effect, while the card
-- draw is not, so we must create a window effect rather than use
-- HasModifiersFor

newtype Daring = Daring SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

daring :: SkillCard Daring
daring = skill Daring Cards.daring

instance RunMessage Daring where
  runMessage msg s@(Daring attrs) = case msg of
    InvestigatorCommittedSkill _ sid | sid == toId attrs -> do
      mtarget <- getSkillTestTarget
      case mtarget of
        Just target@(EnemyTarget _) ->
          push
            $ skillTestModifiers
              (toSource attrs)
              target
              [AddKeyword Keyword.Retaliate, AddKeyword Keyword.Alert]
        _ -> error "Target was invalid"
      pure s
    SkillTestEnds _ _ -> do
      drawing <- drawCards (skillOwner attrs) attrs 1
      push drawing
      pure s
    _ -> Daring <$> runMessage msg attrs
