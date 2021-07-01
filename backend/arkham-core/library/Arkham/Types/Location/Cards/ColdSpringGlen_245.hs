module Arkham.Types.Location.Cards.ColdSpringGlen_245
  ( coldSpringGlen_245
  , ColdSpringGlen_245(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (coldSpringGlen_245)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype ColdSpringGlen_245 = ColdSpringGlen_245 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldSpringGlen_245 :: LocationId -> ColdSpringGlen_245
coldSpringGlen_245 = ColdSpringGlen_245 . baseAttrs
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
ability attrs = mkAbility (toSource attrs) 1 (ReactionAbility Free)

instance ActionRunner env => HasActions env ColdSpringGlen_245 where
  getActions iid (WhenChosenRandomLocation lid) (ColdSpringGlen_245 attrs)
    | lid == toId attrs = pure [ActivateCardAbilityAction iid (ability attrs)]
  getActions iid window (ColdSpringGlen_245 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env ColdSpringGlen_245 where
  runMessage msg l@(ColdSpringGlen_245 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage
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
