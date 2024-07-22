module Arkham.Location.Cards.ColdSpringGlen_245 (
  coldSpringGlen_245,
  ColdSpringGlen_245 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (coldSpringGlen_245)
import Arkham.Location.Runner hiding (ChosenRandomLocation)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype ColdSpringGlen_245 = ColdSpringGlen_245 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldSpringGlen_245 :: LocationCard ColdSpringGlen_245
coldSpringGlen_245 =
  location ColdSpringGlen_245 Cards.coldSpringGlen_245 2 (Static 0)

instance HasModifiersFor ColdSpringGlen_245 where
  getModifiersFor (EnemyTarget eid) (ColdSpringGlen_245 attrs) = do
    atLocation <- enemyAtLocation eid attrs
    pure $ toModifiers attrs [EnemyEvade (-1) | atLocation]
  getModifiersFor _ _ = pure []

instance HasAbilities ColdSpringGlen_245 where
  getAbilities (ColdSpringGlen_245 attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ReactionAbility
            (ChosenRandomLocation Timing.After $ LocationWithId $ toId attrs)
            Free
        | locationRevealed attrs
        ]

instance RunMessage ColdSpringGlen_245 where
  runMessage msg l@(ColdSpringGlen_245 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) attrs SkillAgility (Fixed 3)
      pure l
    PassedSkillTest _ _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ -> do
      replaceMessageMatching
        \case
          Msg.ChosenRandomLocation _ lid -> lid == toId attrs
          _ -> False
        \case
          Msg.ChosenRandomLocation target lid
            | lid == toId attrs ->
                [ChooseRandomLocation target (singleton lid)]
          _ -> error "should be the matching message"
      pure l
    _ -> ColdSpringGlen_245 <$> runMessage msg attrs
