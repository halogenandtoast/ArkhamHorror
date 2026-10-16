{- | "__Spawn__ - Any connecting location (empty if possible)." and the
__Forced__ approach: a clue discovered anywhere, or a test passed as part of an
{action} ability printed on a scenario card, drags the Servant one step toward
the investigator responsible.

The two trigger conditions are two 'HasAbilities' entries rather than one
ability over @oneOf@ windows, because 'forcedLimit' reads the period off the
window constructor: split, the test half takes its correct @PerTest@ bucket,
where a combined window would fall through to @PerWindow@ on a
@SkillTestResult@ -- the shape that loops until the request times out.

The five arrangement locations need no guard of their own. They carry
'DoesNotConnect', and every connection, distance and pathing query resolves a
location's printed connections through @Matcher.LocationWithSymbol@, which drops
a 'DoesNotConnect' location; so they are neither a legal spawn point nor a step
on any path the move can take.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Enemies.ServantOfYogSothoth (
  servantOfYogSothoth,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (DiscoverClues)
import Arkham.ForMovement (ForMovement (NotForMovement))
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTowardsMatching)

newtype ServantOfYogSothoth = ServantOfYogSothoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfYogSothoth :: EnemyCard ServantOfYogSothoth
servantOfYogSothoth =
  enemyWith ServantOfYogSothoth Cards.servantOfYogSothoth
    $ spawnAtL
    ?~ SpawnAtFirst [SpawnAt (connecting <> EmptyLocation), SpawnAt connecting]
 where
  -- "Connecting" is relative to the spawning investigator's location, which is what
  -- ConnectedLocation resolves against.
  connecting = ConnectedLocation NotForMovement

instance HasAbilities ServantOfYogSothoth where
  getAbilities (ServantOfYogSothoth a) =
    extend
      a
      [ scenarioI18n
          $ withI18nTooltip "servantOfYogSothoth.approach"
          $ restricted a 1 readyAndUnengaged
          $ forced
          $ DiscoverClues #after You Anywhere (atLeast 1)
      , scenarioI18n
          $ withI18nTooltip "servantOfYogSothoth.approach"
          $ restricted a 2 readyAndUnengaged
          $ forced
          $ SkillTestResult #after You onAScenarioCardAbility (SuccessResult AnyValue)
      ]
   where
    readyAndUnengaged = exists self
    self :: EnemyMatcher
    self = be a <> #ready <> #unengaged
    -- Prefilter so the Forced is not raised on every test in the game; the handler's
    -- testIsFromScenarioActionAbility stays the authority on "{action} ability printed
    -- on a scenario card", so the scenario keeps one reading of that phrase.
    onAScenarioCardAbility = SkillTestSourceMatches $ SourceIsAbility AbilityOnEncounterCard

instance RunMessage ServantOfYogSothoth where
  runMessage msg e@(ServantOfYogSothoth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTowardsMatching (attrs.ability 1) attrs (locationWithInvestigator iid)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      whenM testIsFromScenarioActionAbility
        $ moveTowardsMatching (attrs.ability 2) attrs (locationWithInvestigator iid)
      pure e
    _ -> ServantOfYogSothoth <$> liftRunMessage msg attrs
