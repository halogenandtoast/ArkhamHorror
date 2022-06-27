module Arkham.Location.Cards.BleakPlainsStarsOfAldebaran
  ( bleakPlainsStarsOfAldebaran
  , BleakPlainsStarsOfAldebaran(..)
  ) where

import Arkham.Prelude

import Arkham.Card.CardType
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Message
import Arkham.Story.Cards qualified as Story
import Arkham.Target
import Arkham.Trait

newtype BleakPlainsStarsOfAldebaran = BleakPlainsStarsOfAldebaran LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bleakPlainsStarsOfAldebaran :: LocationCard BleakPlainsStarsOfAldebaran
bleakPlainsStarsOfAldebaran = locationWith
  BleakPlainsStarsOfAldebaran
  Cards.bleakPlainsStarsOfAldebaran
  4
  (PerPlayer 1)
  Square
  [Circle, Triangle, Diamond]
  (canBeFlippedL .~ True)

instance HasModifiersFor BleakPlainsStarsOfAldebaran where
  getModifiersFor _ (InvestigatorTarget iid) (BleakPlainsStarsOfAldebaran a) =
    pure $ toModifiers
      a
      [ CannotPlay (CardWithType AssetType <> CardWithTrait Ally)
      | iid `member` locationInvestigators a
      ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities BleakPlainsStarsOfAldebaran where
  getAbilities (BleakPlainsStarsOfAldebaran attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BleakPlainsStarsOfAldebaran where
  runMessage msg l@(BleakPlainsStarsOfAldebaran attrs) = case msg of
    Flip _ target | isTarget attrs target -> do
      push $ ReadStory Story.starsOfAldebaran
      pure . BleakPlainsStarsOfAldebaran $ attrs & canBeFlippedL .~ False
    ResolveStory story' | story' == Story.starsOfAldebaran -> do
      iids <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      enemies <- selectList $ NotEnemy $ EnemyWithTitle "Hastur"
      let
        enemyMessages = if null enemies
          then []
          else
            [ chooseOne
                leadInvestigatorId
                [ EnemyDamage
                    enemy
                    leadInvestigatorId
                    (toSource attrs)
                    NonAttackDamageEffect
                    4
                ]
            | enemy <- enemies
            ]
      otherBleakPlains <- selectList $ SetAsideCardMatch $ CardWithTitle "Bleak Plains"
      otherBleakPlain <- case otherBleakPlains of
                           [] -> error "missing"
                           [x] -> pure x
                           (x:xs) -> sample (x :| xs)
      pushAll
        $ [ HealHorror (InvestigatorTarget iid) 3 | iid <- iids ]
        <> enemyMessages
        <> [ReplaceLocation (toId attrs) otherBleakPlain]
      pure l
    _ -> BleakPlainsStarsOfAldebaran <$> runMessage msg attrs
