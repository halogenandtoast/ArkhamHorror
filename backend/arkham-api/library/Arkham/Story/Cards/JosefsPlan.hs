module Arkham.Story.Cards.JosefsPlan (JosefsPlan (..), josefsPlan, josefsPlanEffect) where

import Arkham.Prelude

import Arkham.Effect.Import
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
    ResolveStory _ _ story' | story' == toId attrs -> do
      josefMeiger <- selectJust $ enemyIs Enemies.josefMeiger
      enabled <- createCardEffect Cards.josefsPlan Nothing attrs attrs
      pushAll
        [ enabled
        , RemoveAllDoom (toSource attrs) (toTarget josefMeiger)
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
  getModifiersFor (JosefsPlanEffect a) = do
    silverTwilight <- modifySelect a (EnemyWithTrait SilverTwilight) [CannotPlaceDoomOnThis]
    josef <- modifySelect a (enemyIs Enemies.josefMeiger) [AddKeyword Keyword.Aloof]
    pure $ silverTwilight <> josef

instance RunMessage JosefsPlanEffect where
  runMessage msg (JosefsPlanEffect attrs) =
    JosefsPlanEffect <$> runMessage msg attrs
