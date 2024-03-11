module Arkham.Location.Cards.Sarnath (sarnath, Sarnath (..)) where

import Arkham.Constants
import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Story

newtype Sarnath = Sarnath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sarnath :: LocationCard Sarnath
sarnath = location Sarnath Cards.sarnath 3 (PerPlayer 1)

instance HasAbilities Sarnath where
  getAbilities (Sarnath attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility
          attrs
          VeiledAbility
          ( exists (LocationWithId (toId attrs) <> LocationCanBeFlipped <> LocationWithoutClues)
              <> Remembered KnowWhatHappenedToIb
          )
          (FastAbility Free)
      , mkAbility attrs 1
          $ ForcedAbility
          $ SkillTestResult
            #after
            You
            (WhileInvestigating $ LocationWithId $ toId attrs)
            (SuccessResult AnyValue)
      ]

instance RunMessage Sarnath where
  runMessage msg l@(Sarnath attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theDoomOfSarnath
      pure . Sarnath $ attrs & canBeFlippedL .~ False
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> Sarnath <$> runMessage msg attrs
