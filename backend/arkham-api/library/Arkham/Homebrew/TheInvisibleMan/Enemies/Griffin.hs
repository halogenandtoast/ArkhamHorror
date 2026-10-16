module Arkham.Homebrew.TheInvisibleMan.Enemies.Griffin (griffin) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers hiding (griffin)
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher
import Arkham.Projection

newtype Griffin = Griffin EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

griffin :: EnemyCard Griffin
griffin = enemy Griffin Cards.griffin

instance HasAbilities Griffin where
  getAbilities (Griffin a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "griffin.prowl"
      $ mkAbility a 1
      $ forced
      $ PhaseBegins #when #enemy

instance RunMessage Griffin where
  runMessage msg e@(Griffin attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      field EnemyLocation attrs.id >>= traverse_ \lid -> do
        n <- perPlayer 1
        clues <- field LocationClues lid
        when (clues < n) $ placeClues (attrs.ability 1) lid (n - clues)
      whenM (attrs.id <=~> ReadyEnemy) do
        selectEach (investigatorEngagedWith attrs) $ initiateEnemyAttack attrs (attrs.ability 1)
        doStep 1 msg
      pure e
    -- The destination is read once the attacks have landed, so a defeated
    -- investigator no longer holds a location away from him.
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      griffinMoves (attrs.ability 1) attrs.id
      pure e
    -- Acts 1d and 1f set these "for the remainder of the scenario", which is
    -- what advancing their deck means. Written as the printed value, not an
    -- 'EnemyFight' modifier: 'Arkham.Game' reads the field as a 'Maybe' and
    -- returns 'Nothing' before applying any modifier, so a modifier on a card
    -- printing "?" is silently dropped.
    AdvanceActDeck _ (isAct Acts.theQuestionOfFighting -> True) ->
      pure . Griffin $ attrs & fightL ?~ setValue
    AdvanceActDeck _ (isAct Acts.theQuestionOfFleeing -> True) ->
      pure . Griffin $ attrs & evadeL ?~ setValue
    _ -> Griffin <$> liftRunMessage msg attrs

-- | "set Griffin's fight value to 3" -- both acts set the same number.
setValue :: GameCalculation
setValue = GameValueCalculation (Static 3)
