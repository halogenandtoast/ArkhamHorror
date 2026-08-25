module Arkham.Homebrew.DarkMatter.Enemies.TheEntity (theEntity) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Strategy

newtype TheEntity = TheEntity EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

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
    {- "+2[per_investigator] health for each facedown card attached to it". Act 2b
    and agenda 4a are the only things that attach cards here, and they do it in
    two shapes: a crew member still in play survives as an asset with an
    'AttachedToEnemy' placement, while one that is only a card (removed from the
    game, or still in the scanning deck) is placed facedown underneath. -}
    attached <- selectCount $ EnemyAsset a.id
    bonus <- perPlayer (2 * (attached + length (enemyCardsUnderneath a)))
    modifySelf a
      $ [AddKeyword Keyword.Massive, AddKeyword Keyword.Hunter, AddKeyword Keyword.Alert]
      <> [HealthModifier bonus | bonus > 0]

instance RunMessage TheEntity where
  runMessage msg (TheEntity attrs) = TheEntity <$> runMessage msg attrs
