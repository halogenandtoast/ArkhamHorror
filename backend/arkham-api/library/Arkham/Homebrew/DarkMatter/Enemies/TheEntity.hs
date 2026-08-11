module Arkham.Homebrew.DarkMatter.Enemies.TheEntity (theEntity) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Strategy
import Arkham.Treachery.Types (Field (TreacheryPlacement))

newtype TheEntity = TheEntity EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Damage and horror dealt by The Entity must be assigned to [[Ally]] assets
first." — an attack damage strategy, not a modifier.
-}
theEntity :: EnemyCard TheEntity
theEntity =
  enemyWith TheEntity Cards.theEntity
    $ damageStrategyL
    .~ DamageAndHorrorAssetsFirst #ally

{- | "Massive. Hunter. Alert. / The Entity gets +2[per_investigator] health for
each facedown card attached to it. / Damage and horror dealt by The Entity must
be assigned to [[Ally]] assets first."
-}
instance HasModifiersFor TheEntity where
  getModifiersFor (TheEntity a) = do
    {- "+2[per_investigator] health for each facedown card attached to it". No
    matcher exists for treacheries attached to an *enemy*, so the attached cards
    are counted from their placements. -}
    attached <- select AnyTreachery
    placements <- traverse (field TreacheryPlacement) attached
    let facedown = count (== AttachedToEnemy a.id) placements
    bonus <- perPlayer (2 * facedown)
    modifySelf a
      $ [AddKeyword Keyword.Massive, AddKeyword Keyword.Hunter, AddKeyword Keyword.Alert]
      <> [HealthModifier bonus | bonus > 0]

instance RunMessage TheEntity where
  runMessage msg (TheEntity attrs) = TheEntity <$> runMessage msg attrs
