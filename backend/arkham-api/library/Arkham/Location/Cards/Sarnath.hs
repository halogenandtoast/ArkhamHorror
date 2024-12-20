module Arkham.Location.Cards.Sarnath (sarnath) where

import Arkham.Ability
import Arkham.Constants
import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Story

newtype Sarnath = Sarnath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sarnath :: LocationCard Sarnath
sarnath = location Sarnath Cards.sarnath 3 (PerPlayer 1)

instance HasAbilities Sarnath where
  getAbilities (Sarnath a) =
    extendRevealed
      a
      [ restricted
          a
          VeiledAbility
          ( exists (be a <> LocationCanBeFlipped <> LocationWithoutClues)
              <> Remembered KnowWhatHappenedToIb
          )
          (FastAbility Free)
      , mkAbility a 1 $ forced $ SkillTestResult #after You (WhileInvestigating $ be a) #success
      ]

instance RunMessage Sarnath where
  runMessage msg l@(Sarnath attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theDoomOfSarnath
      pure . Sarnath $ attrs & canBeFlippedL .~ False
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> Sarnath <$> liftRunMessage msg attrs
