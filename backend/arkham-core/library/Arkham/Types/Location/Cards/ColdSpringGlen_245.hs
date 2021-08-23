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
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

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

ability :: LocationAttrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (LegacyReactionAbility Free)

instance HasAbilities env ColdSpringGlen_245 where
  getAbilities _ (Window Timing.When (Window.ChosenRandomLocation lid)) (ColdSpringGlen_245 attrs)
    | lid == toId attrs
    = pure [locationAbility (ability attrs)]
  getAbilities iid window (ColdSpringGlen_245 attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env ColdSpringGlen_245 where
  runMessage msg l@(ColdSpringGlen_245 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push
        (BeginSkillTest iid source (toTarget attrs) Nothing SkillAgility 3)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ replaceMessageMatching
        (\case
          ChosenRandomLocation _ lid | lid == toId attrs -> True
          _ -> False
        )
        (\case
          ChosenRandomLocation target lid | lid == toId attrs ->
            [ChooseRandomLocation target (singleton lid)]
          _ -> error "should be the matching message"
        )
    _ -> ColdSpringGlen_245 <$> runMessage msg attrs
