module Arkham.Homebrew.CircusExMortis.Enemies.TheCultEnMasseLeaderlessFanaticism (
  theCultEnMasseLeaderlessFanaticism,
  cultEnMasseModifiers,
) where

import Arkham.Card.CardType (CardType (ActType))
import Arkham.ChaosBag.Base (chaosBagPendingRequests)
import Arkham.ChaosToken.Types (ChaosToken)
import Arkham.Classes.HasGame
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.ChaosBag (getChaosBag)
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach, modifySelf)
import Arkham.Helpers.SkillTest (getSkillTest, skillTestMatches)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Matcher
import Data.Map.Strict qualified as Map

newtype TheCultEnMasseLeaderlessFanaticism = TheCultEnMasseLeaderlessFanaticism EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCultEnMasseLeaderlessFanaticism :: EnemyCard TheCultEnMasseLeaderlessFanaticism
theCultEnMasseLeaderlessFanaticism =
  enemy TheCultEnMasseLeaderlessFanaticism Cards.theCultEnMasseLeaderlessFanaticism

{- | Everything the three The Cult En Masse variants print in common; they differ
only in keywords and stats, so the other two import this.
-}
cultEnMasseModifiers :: HasModifiersM m => EnemyAttrs -> m ()
cultEnMasseModifiers a = do
  modifySelf a [CannotMove, CannotBeMoved]
  moons <- if a.ready then substitutedMoonTokens a else pure []
  modifyEach a moons [ForcedChaosTokenChange MoonToken [#autofail]]

{- | The revealed moon tokens currently treated as auto-fail: only in the three
printed contexts -- an ability on the act, or an attack or an investigation at
this enemy's location.
-}
substitutedMoonTokens :: HasGame m => EnemyAttrs -> m [ChaosToken]
substitutedMoonTokens a = do
  bag <- getChaosBag
  case filter ((== MoonToken) . (.face)) bag.revealed of
    [] -> pure []
    moons ->
      getSkillTest >>= \case
        Just st -> do
          inContext <- skillTestMatches st.investigator (toSource a) st contexts
          pure $ if inContext then filter (`elem` st.revealedChaosTokens) moons else []
        -- Outside a skill test this clause only covers the act's own ability, so
        -- the source that asked for the tokens has to be the act. Sealing a moon
        -- token reveals it the same way, and must not be turned into an auto-fail.
        Nothing -> do
          onAct <- anyM (`sourceMatches` SourceIsType ActType) (Map.keys $ chaosBagPendingRequests bag)
          pure $ if onAct then moons else []
 where
  here = locationWithEnemy a
  contexts =
    SkillTestOneOf
      [ SkillTestMatches [WhileAttacking, SkillTestOfInvestigator (InvestigatorAt here)]
      , WhileInvestigating here
      ]

instance HasModifiersFor TheCultEnMasseLeaderlessFanaticism where
  getModifiersFor (TheCultEnMasseLeaderlessFanaticism a) = cultEnMasseModifiers a

instance RunMessage TheCultEnMasseLeaderlessFanaticism where
  runMessage msg (TheCultEnMasseLeaderlessFanaticism attrs) =
    runQueueT $ TheCultEnMasseLeaderlessFanaticism <$> liftRunMessage msg attrs
