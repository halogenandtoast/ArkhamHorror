module Arkham.Location.Cards.ColdSpringGlen_245
  ( coldSpringGlen_245
  , ColdSpringGlen_245(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( coldSpringGlen_245 )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( ChosenRandomLocation )
import Arkham.Message qualified as Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype ColdSpringGlen_245 = ColdSpringGlen_245 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldSpringGlen_245 :: LocationCard ColdSpringGlen_245
coldSpringGlen_245 =
  location ColdSpringGlen_245 Cards.coldSpringGlen_245 2 (Static 0)

instance HasModifiersFor ColdSpringGlen_245 where
  getModifiersFor (EnemyTarget eid) (ColdSpringGlen_245 attrs) =
    pure $ toModifiers
      attrs
      [ EnemyEvade (-1) | eid `elem` locationEnemies attrs ]
  getModifiersFor _ _ = pure []

instance HasAbilities ColdSpringGlen_245 where
  getAbilities (ColdSpringGlen_245 attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1 $ ReactionAbility
            (ChosenRandomLocation Timing.After $ LocationWithId $ toId attrs)
            Free
        | locationRevealed attrs
        ]

instance RunMessage ColdSpringGlen_245 where
  runMessage msg l@(ColdSpringGlen_245 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
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
