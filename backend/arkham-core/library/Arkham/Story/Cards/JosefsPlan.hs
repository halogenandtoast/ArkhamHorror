module Arkham.Story.Cards.JosefsPlan (
  JosefsPlan (..),
  josefsPlan,
  josefsPlanEffect,
) where

import Arkham.Prelude

import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Arkham.Trait (Trait (SilverTwilight))

newtype JosefsPlan = JosefsPlan StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

josefsPlan :: StoryCard JosefsPlan
josefsPlan = story JosefsPlan Cards.josefsPlan

instance RunMessage JosefsPlan where
  runMessage msg s@(JosefsPlan attrs) = case msg of
    ResolveStory _ story' | story' == toId attrs -> do
      josefMeiger <- selectJust $ enemyIs Enemies.josefMeiger
      pushAll
        [ createCardEffect Cards.josefsPlan Nothing attrs attrs
        , RemoveAllDoom (toTarget josefMeiger)
        , DisengageEnemyFromAll josefMeiger
        ]
      pure s
    _ -> JosefsPlan <$> runMessage msg attrs

newtype JosefsPlanEffect = JosefsPlanEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

josefsPlanEffect :: EffectArgs -> JosefsPlanEffect
josefsPlanEffect = cardEffect JosefsPlanEffect Cards.josefsPlan

instance HasModifiersFor JosefsPlanEffect where
  getModifiersFor (EnemyTarget eid) (JosefsPlanEffect a) = do
    isSilverTwilight <- eid <=~> EnemyWithTrait SilverTwilight
    isJosefMeiger <- eid <=~> enemyIs Enemies.josefMeiger
    pure $
      toModifiers a $
        [CannotPlaceDoomOnThis | isSilverTwilight]
          <> [AddKeyword Keyword.Aloof | isJosefMeiger]
  getModifiersFor _ _ = pure []

instance RunMessage JosefsPlanEffect where
  runMessage msg (JosefsPlanEffect attrs) =
    JosefsPlanEffect <$> runMessage msg attrs
