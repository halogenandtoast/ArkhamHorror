module Arkham.Enemy.Cards.Cnidathqua (
  cnidathqua,
  Cnidathqua (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype Cnidathqua = Cnidathqua EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cnidathqua :: EnemyCard Cnidathqua
cnidathqua =
  enemyWith
    Cnidathqua
    Cards.cnidathqua
    (4, PerPlayer 8, 0)
    (2, 2)
    ((asSelfLocationL ?~ "cnidathqua") . (evadeL .~ Nothing))

instance HasModifiersFor Cnidathqua where
  getModifiersFor (Cnidathqua attrs) =
    modifySelf attrs [CannotBeEvaded, CanBeFoughtAsIfAtYourLocation]

instance HasAbilities Cnidathqua where
  getAbilities (Cnidathqua attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ SkillTestResult
            Timing.After
            You
            (WhileAttackingAnEnemy $ EnemyWithId $ toId attrs)
          $ FailureResult AnyValue
      , mkAbility attrs 2
          $ Objective
          $ ForcedAbility
          $ EnemyDefeated Timing.When Anyone ByAny
          $ EnemyWithId
          $ toId attrs
      ]

instance RunMessage Cnidathqua where
  runMessage msg e@(Cnidathqua attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push
        $ FindEncounterCard
          iid
          (toTarget attrs)
          [FromEncounterDeck, FromEncounterDiscard]
          (CardWithTitle "Writhing Appendage")
      pure e
    FoundEncounterCard iid target card | isTarget attrs target -> do
      lid <- getJustLocation iid
      e <$ push (SpawnEnemyAtEngagedWith (EncounterCard card) lid iid)
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      e <$ push (scenarioResolution 2)
    _ -> Cnidathqua <$> runMessage msg attrs
