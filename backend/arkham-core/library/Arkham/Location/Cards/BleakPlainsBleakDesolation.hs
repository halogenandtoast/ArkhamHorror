module Arkham.Location.Cards.BleakPlainsBleakDesolation
  ( bleakPlainsBleakDesolation
  , BleakPlainsBleakDesolation(..)
  ) where

import Arkham.Prelude

import Arkham.Card.CardType
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Message
import Arkham.Story.Cards qualified as Story
import Arkham.Target
import Arkham.Trait

newtype BleakPlainsBleakDesolation = BleakPlainsBleakDesolation LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bleakPlainsBleakDesolation :: LocationCard BleakPlainsBleakDesolation
bleakPlainsBleakDesolation = locationWith
  BleakPlainsBleakDesolation
  Cards.bleakPlainsBleakDesolation
  4
  (PerPlayer 1)
  Square
  [Circle, Triangle, Diamond]
  (canBeFlippedL .~ True)

instance HasModifiersFor BleakPlainsBleakDesolation where
  getModifiersFor _ (InvestigatorTarget iid) (BleakPlainsBleakDesolation a) =
    pure $ toModifiers
      a
      [ CannotPlay (CardWithType AssetType <> CardWithTrait Ally)
      | iid `member` locationInvestigators a
      ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities BleakPlainsBleakDesolation where
  getAbilities (BleakPlainsBleakDesolation attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BleakPlainsBleakDesolation where
  runMessage msg l@(BleakPlainsBleakDesolation attrs) = case msg of
    Flip _ target | isTarget attrs target -> do
      push $ ReadStory Story.bleakDesolation
      pure . BleakPlainsBleakDesolation $ attrs & canBeFlippedL .~ False
    ResolveStory story' | story' == Story.bleakDesolation -> do
      leadInvestigatorId <- getLeadInvestigatorId
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- getPlayerCountValue (PerPlayer 2)
      push $ EnemyDamage
        hastur
        leadInvestigatorId
        (toSource attrs)
        NonAttackDamageEffect
        n
      pure l
    _ -> BleakPlainsBleakDesolation <$> runMessage msg attrs
