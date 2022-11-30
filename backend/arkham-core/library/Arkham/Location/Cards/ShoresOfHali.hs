module Arkham.Location.Cards.ShoresOfHali
  ( shoresOfHali
  , ShoresOfHali(..)
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
import Arkham.Target

newtype ShoresOfHali = ShoresOfHali LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

shoresOfHali :: LocationCard ShoresOfHali
shoresOfHali = locationWith
  ShoresOfHali
  Cards.shoresOfHali
  3
  (PerPlayer 2)
  ((canBeFlippedL .~ True) . (revealedL .~ True))

instance RunMessage ShoresOfHali where
  runMessage msg l@(ShoresOfHali attrs) = case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.songsThatTheHyadesShallSing
      pure . ShoresOfHali $ attrs & canBeFlippedL .~ False
    ResolveStory iid story' | story' == Story.songsThatTheHyadesShallSing -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      investigatorIds <- selectList $ investigatorEngagedWith hastur
      n <- getPlayerCountValue (PerPlayer 1)
      pushAll
        $ [ EnemyDamage hastur (InvestigatorSource iid) NonAttackDamageEffect n
          , Exhaust (EnemyTarget hastur)
          ]
        <> [ DisengageEnemy iid' hastur | iid' <- investigatorIds ]
      pure l
    _ -> ShoresOfHali <$> runMessage msg attrs
