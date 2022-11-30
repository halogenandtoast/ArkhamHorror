module Arkham.Location.Cards.DarkSpires
  ( darkSpires
  , DarkSpires(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Message
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Source
import Arkham.Story.Cards qualified as Story

newtype DarkSpires = DarkSpires LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

darkSpires :: LocationCard DarkSpires
darkSpires = locationWith
  DarkSpires
  Cards.darkSpires
  3
  (PerPlayer 2)
  ((canBeFlippedL .~ True) . (revealedL .~ True))

instance RunMessage DarkSpires where
  runMessage msg l@(DarkSpires attrs) = case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.theFall
      pure . DarkSpires $ attrs & canBeFlippedL .~ False
    ResolveStory iid story' | story' == Story.stepsOfThePalace -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- getPlayerCountValue (PerPlayer 2)
      push $ chooseOne
        iid
        [ Label
          "You cannot bring yourseld to do what must be done."
          [UpdateLocation (attrs & canBeFlippedL .~ True) (toId attrs)]
        , Label
          "Realizing what you must do, you step forward and push her."
          [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2
          , EnemyDamage hastur (InvestigatorSource iid) StoryCardDamageEffect n
          ]
        ]
      pure l
    _ -> DarkSpires <$> runMessage msg attrs
