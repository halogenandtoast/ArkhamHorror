module Arkham.Homebrew.AgainstTheWendigo.Enemies.TheWendigo (theWendigo) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Cards
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Matcher qualified as Matcher

newtype TheWendigo = TheWendigo EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Prey - The investigator with the most damage on them and their Ally cards."
The Ally half is not expressible as a prey matcher; the printed tie-break the
engine can see is most damage.
-}
theWendigo :: EnemyCard TheWendigo
theWendigo =
  enemy TheWendigo Cards.theWendigo
    & setPrey MostDamage
    & setSpawnAt (connectedFrom $ LocationWithInvestigator MostDamage)

instance HasModifiersFor TheWendigo where
  -- "The Wendigo gets +4 [per_investigator] health."
  getModifiersFor (TheWendigo a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier (4 * n)]

instance HasAbilities TheWendigo where
  getAbilities (TheWendigo a) =
    [ -- "At the end of each investigator turn, ready The Wendigo."
      restricted a 1 (exists $ be a <> ExhaustedEnemy) $ forced $ TurnEnds #when Anyone
    , -- "If a doom token is put into play, heal 1 damage on The Wendigo (for a 3
      -- or 4 player game, heal 2 damage instead)."
      restricted a 2 (exists $ be a <> EnemyWithAnyDamage) $ forced $ PlacedDoomCounter #after AnySource AnyTarget
    , -- "If an investigator is defeated at The Wendigo's location, he or she
      -- suffers 1 additional physical trauma."
      restricted a 3 Here $ forced $ Matcher.InvestigatorDefeated #when ByAny (InvestigatorAt $ locationWithEnemy a)
    ]

instance RunMessage TheWendigo where
  runMessage msg e@(TheWendigo attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      readyThis attrs
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      n <- getPlayerCount
      healDamage attrs (attrs.ability 2) (if n >= 3 then 2 else 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      sufferPhysicalTrauma iid 1
      pure e
    _ -> TheWendigo <$> liftRunMessage msg attrs
