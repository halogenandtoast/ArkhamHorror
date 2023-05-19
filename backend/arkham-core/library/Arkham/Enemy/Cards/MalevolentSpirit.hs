module Arkham.Enemy.Cards.MalevolentSpirit (
  malevolentSpirit,
  MalevolentSpirit (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword (Keyword (Hunter))
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Relic, Spectral, Spell))

newtype MalevolentSpirit = MalevolentSpirit EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

malevolentSpirit :: EnemyCard MalevolentSpirit
malevolentSpirit =
  enemyWith
    MalevolentSpirit
    Cards.malevolentSpirit
    (2, Static 2, 4)
    (0, 1)
    ( spawnAtL
        ?~ SpawnLocation
          (LocationMatchAny [LocationWithTitle "Chapel Attic", LocationWithTitle "Chapel Crypt"])
    )

instance HasModifiersFor MalevolentSpirit where
  getModifiersFor target (MalevolentSpirit a) | isTarget a target = do
    atSpectral <- selectAny $ locationWithEnemy (toId a) <> LocationWithTrait Spectral
    pure $ toModifiers a $ if atSpectral then [AddKeyword Hunter, DamageDealt 1, HorrorDealt 1] else []
  getModifiersFor _ _ = pure []

instance HasAbilities MalevolentSpirit where
  getAbilities (MalevolentSpirit a) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ EnemyDefeated
            Timing.When
            Anyone
            (BySource $ NotSource $ SourceMatchesAny [SourceWithTrait Spell, SourceWithTrait Relic])
          $ EnemyWithId
          $ toId a
      ]

instance RunMessage MalevolentSpirit where
  runMessage msg e@(MalevolentSpirit attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      spectralLocations <- selectList $ LocationWithTrait Spectral
      lead <- getLead
      pushAll $
        [ CancelNext (toSource attrs) EnemyDefeatedMessage
        , HealAllDamage (toTarget attrs) (toSource attrs)
        , DisengageEnemyFromAll (toId attrs)
        , Exhaust (toTarget attrs)
        ]
          <> [chooseOrRunOne lead [targetLabel lid [EnemyMove (toId attrs) lid]] | lid <- spectralLocations]
      pure e
    _ -> MalevolentSpirit <$> runMessage msg attrs
