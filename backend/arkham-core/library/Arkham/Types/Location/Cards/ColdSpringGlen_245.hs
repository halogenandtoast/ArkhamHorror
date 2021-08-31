module Arkham.Types.Location.Cards.ColdSpringGlen_245
  ( coldSpringGlen_245
  , ColdSpringGlen_245(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (coldSpringGlen_245)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (ChosenRandomLocation)
import qualified Arkham.Types.Message as Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype ColdSpringGlen_245 = ColdSpringGlen_245 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldSpringGlen_245 :: LocationCard ColdSpringGlen_245
coldSpringGlen_245 = location
  ColdSpringGlen_245
  Cards.coldSpringGlen_245
  2
  (Static 0)
  Triangle
  [Circle, Diamond, Plus]

instance HasModifiersFor env ColdSpringGlen_245 where
  getModifiersFor _ (EnemyTarget eid) (ColdSpringGlen_245 attrs) =
    pure $ toModifiers
      attrs
      [ EnemyEvade (-1) | eid `elem` locationEnemies attrs ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities env ColdSpringGlen_245 where
  getAbilities iid window (ColdSpringGlen_245 attrs) =
    withBaseAbilities iid window attrs $ pure
      [ mkAbility attrs 1 $ ReactionAbility
          (ChosenRandomLocation Timing.After $ LocationWithId $ toId attrs)
          Free
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env ColdSpringGlen_245 where
  runMessage msg l@(ColdSpringGlen_245 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push
        (BeginSkillTest iid source (toTarget attrs) Nothing SkillAgility 3)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ replaceMessageMatching
        (\case
          Message.ChosenRandomLocation _ lid | lid == toId attrs -> True
          _ -> False
        )
        (\case
          Message.ChosenRandomLocation target lid | lid == toId attrs ->
            [ChooseRandomLocation target (singleton lid)]
          _ -> error "should be the matching message"
        )
    _ -> ColdSpringGlen_245 <$> runMessage msg attrs
