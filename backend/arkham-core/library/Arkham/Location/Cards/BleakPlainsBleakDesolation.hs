module Arkham.Location.Cards.BleakPlainsBleakDesolation
  ( bleakPlainsBleakDesolation
  , BleakPlainsBleakDesolation(..)
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

newtype BleakPlainsBleakDesolation = BleakPlainsBleakDesolation LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bleakPlainsBleakDesolation :: LocationCard BleakPlainsBleakDesolation
bleakPlainsBleakDesolation = locationWith
  BleakPlainsBleakDesolation
  Cards.bleakPlainsBleakDesolation
  4
  (PerPlayer 1)
  ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasModifiersFor BleakPlainsBleakDesolation where
  getModifiersFor (InvestigatorTarget iid) (BleakPlainsBleakDesolation a) =
    pure $ toModifiers a [ CannotPlay IsAlly | iid `on` a ]
  getModifiersFor _ _ = pure []

instance RunMessage BleakPlainsBleakDesolation where
  runMessage msg l@(BleakPlainsBleakDesolation attrs) = case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.bleakDesolation
      pure . BleakPlainsBleakDesolation $ attrs & canBeFlippedL .~ False
    ResolveStory iid story' | story' == Story.bleakDesolation -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- getPlayerCountValue (PerPlayer 2)
      push $ EnemyDamage hastur $ storyDamage (InvestigatorSource iid) n
      pure l
    _ -> BleakPlainsBleakDesolation <$> runMessage msg attrs
