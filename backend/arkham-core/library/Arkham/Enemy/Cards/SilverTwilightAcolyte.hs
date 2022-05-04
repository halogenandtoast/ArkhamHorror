module Arkham.Enemy.Cards.SilverTwilightAcolyte
  ( SilverTwilightAcolyte(..)
  , silverTwilightAcolyte
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding (EnemyAttacks)
import Arkham.Timing qualified as Timing

newtype SilverTwilightAcolyte = SilverTwilightAcolyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTwilightAcolyte :: EnemyCard SilverTwilightAcolyte
silverTwilightAcolyte = enemyWith
  SilverTwilightAcolyte
  Cards.silverTwilightAcolyte
  (2, Static 3, 3)
  (1, 0)
  (preyL .~ Bearer)

instance HasAbilities SilverTwilightAcolyte where
  getAbilities (SilverTwilightAcolyte a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ EnemyAttacks Timing.After Anyone
      $ EnemyWithId
      $ toId a
    ]

instance EnemyRunner env => RunMessage env SilverTwilightAcolyte where
  runMessage msg e@(SilverTwilightAcolyte attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      e <$ push PlaceDoomOnAgenda
    _ -> SilverTwilightAcolyte <$> runMessage msg attrs
