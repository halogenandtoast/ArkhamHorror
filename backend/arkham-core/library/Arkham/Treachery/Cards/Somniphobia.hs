module Arkham.Treachery.Cards.Somniphobia (
  somniphobia,
  Somniphobia (..),
)
where

import Arkham.Card
import Arkham.Classes
import Arkham.Message
import Arkham.Prelude
import Arkham.SkillTest.Base
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Somniphobia = Somniphobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somniphobia :: TreacheryCard Somniphobia
somniphobia = treachery Somniphobia Cards.somniphobia

instance RunMessage Somniphobia where
  runMessage msg t@(Somniphobia attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- can't revelation because we need to update the skills map
      -- push $ revelationSkillTest iid attrs #willpower 5

      let card = toCard attrs
      let skillTest =
            (initSkillTest iid attrs iid #willpower (SkillTestDifficulty $ Fixed 5))
              { skillTestIsRevelation = True
              }
      let
        skillTest' =
          skillTest
            { skillTestIconValues =
                mapFromList [(#intellect, 1), (#combat, 1), (#agility, 1), (#willpower, 2), (#wild, 2)]
                  <> skillTestIconValues skillTest
            }
      pushAll [SetActiveCard card, BeginSkillTest skillTest', UnsetActiveCard]
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) (min 3 -> n) -> do
      push $ assignHorror iid attrs n
      pure t
    _ -> Somniphobia <$> runMessage msg attrs
