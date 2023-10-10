module Arkham.Location.Cards.TheBlackThrone (
  theBlackThrone,
  TheBlackThrone (..),
)
where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype TheBlackThrone = TheBlackThrone LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackThrone :: LocationCard TheBlackThrone
theBlackThrone =
  locationWith
    TheBlackThrone
    Cards.theBlackThrone
    1
    (PerPlayer 2)
    (connectsToL .~ adjacentLocations)

instance HasModifiersFor TheBlackThrone where
  getModifiersFor target (TheBlackThrone attrs) | isTarget attrs target = do
    azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
    doom <- field EnemyDoom azathoth
    let x = ceiling (fromIntegral doom / (2 :: Double))
    pure $ toModifiers attrs [ShroudModifier x]
  getModifiersFor _ _ = pure []

instance HasAbilities TheBlackThrone where
  getAbilities (TheBlackThrone attrs) =
    withRevealedAbilities
      attrs
      [haunted "You must either place 1 doom on Azathoth, or Azathoth attacks you" attrs 1]

instance RunMessage TheBlackThrone where
  runMessage msg l@(TheBlackThrone attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Place 1 doom on Azathoth" [PlaceDoom (toAbilitySource attrs 1) (toTarget azathoth) 1]
          , Label "Azathoth attacks you" [toMessage $ enemyAttack azathoth (toAbilitySource attrs 1) iid]
          ]
      pure l
    _ -> TheBlackThrone <$> runMessage msg attrs
