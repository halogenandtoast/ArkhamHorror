module Arkham.Location.Cards.BleakPlainsStarsOfAldebaran
  ( bleakPlainsStarsOfAldebaran
  , BleakPlainsStarsOfAldebaran(..)
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

newtype BleakPlainsStarsOfAldebaran = BleakPlainsStarsOfAldebaran LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bleakPlainsStarsOfAldebaran :: LocationCard BleakPlainsStarsOfAldebaran
bleakPlainsStarsOfAldebaran = locationWith
  BleakPlainsStarsOfAldebaran
  Cards.bleakPlainsStarsOfAldebaran
  4
  (PerPlayer 1)
  ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasModifiersFor BleakPlainsStarsOfAldebaran where
  getModifiersFor (InvestigatorTarget iid) (BleakPlainsStarsOfAldebaran a) =
    pure $ toModifiers a [ CannotPlay IsAlly | iid `on` a ]
  getModifiersFor _ _ = pure []

instance RunMessage BleakPlainsStarsOfAldebaran where
  runMessage msg l@(BleakPlainsStarsOfAldebaran attrs) = case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.starsOfAldebaran
      pure . BleakPlainsStarsOfAldebaran $ attrs & canBeFlippedL .~ False
    ResolveStory iid story' | story' == Story.starsOfAldebaran -> do
      iids <- getInvestigatorIds
      enemies <- selectList $ NotEnemy $ EnemyWithTitle "Hastur"
      let
        damageEnemy enemy = targetLabel
          enemy
          [EnemyDamage enemy (InvestigatorSource iid) NonAttackDamageEffect 4]
      setAsideBleakPlains <- getSetAsideCardsMatching
        $ CardWithTitle "Bleak Plains"
      otherBleakPlain <- case setAsideBleakPlains of
        [] -> error "missing"
        (x : xs) -> sample (x :| xs)
      pushAll
        $ [ HealHorror (InvestigatorTarget iid') 3 | iid' <- iids ]
        <> [ chooseOrRunOne iid $ map damageEnemy enemies | notNull enemies ]
        <> [ReplaceLocation (toId attrs) otherBleakPlain]
      pure l
    _ -> BleakPlainsStarsOfAldebaran <$> runMessage msg attrs
