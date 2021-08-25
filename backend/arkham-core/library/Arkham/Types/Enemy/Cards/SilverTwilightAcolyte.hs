module Arkham.Types.Enemy.Cards.SilverTwilightAcolyte
  ( SilverTwilightAcolyte(..)
  , silverTwilightAcolyte
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyAttacks)
import Arkham.Types.Prey
import qualified Arkham.Types.Timing as Timing

newtype SilverTwilightAcolyte = SilverTwilightAcolyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTwilightAcolyte :: EnemyCard SilverTwilightAcolyte
silverTwilightAcolyte = enemyWith
  SilverTwilightAcolyte
  Cards.silverTwilightAcolyte
  (2, Static 3, 3)
  (1, 0)
  (preyL .~ SetToBearer)

instance HasAbilities env SilverTwilightAcolyte where
  getAbilities iid window (SilverTwilightAcolyte a) =
    withBaseAbilities iid window a $ pure
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
