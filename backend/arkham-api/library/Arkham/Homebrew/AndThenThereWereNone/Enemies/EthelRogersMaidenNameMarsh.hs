{- | The Housekeeper, unmasked. The sigils she feeds every night price the party
out of its own hands.
-}
module Arkham.Homebrew.AndThenThereWereNone.Enemies.EthelRogersMaidenNameMarsh (
  ethelRogersMaidenNameMarsh,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.GameEnv (getPhase)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMap)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher
import Arkham.Phase

newtype EthelRogersMaidenNameMarsh = EthelRogersMaidenNameMarsh EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ethelRogersMaidenNameMarsh :: EnemyCard EthelRogersMaidenNameMarsh
ethelRogersMaidenNameMarsh = enemy EthelRogersMaidenNameMarsh Cards.ethelRogersMaidenNameMarsh

instance HasModifiersFor EthelRogersMaidenNameMarsh where
  -- "During the investigation phase, increase the cost of each card in a
  -- player's hand by 1 for every 2{perPlayer} total offerings among copies of
  -- Antediluvian Sigils."
  getModifiersFor (EthelRogersMaidenNameMarsh a) = do
    phase <- getPhase
    when (phase == InvestigationPhase) do
      total <- totalOfferings
      per <- perPlayer 2
      let n = if per > 0 then total `div` per else 0
      when (n > 0)
        $ modifySelectMap a Anyone \iid ->
          [IncreaseCostOf (InHandOf NotForPlay $ InvestigatorWithId iid) n]

instance HasAbilities EthelRogersMaidenNameMarsh where
  getAbilities (EthelRogersMaidenNameMarsh a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "ethelRogersMaidenNameMarsh.offerings"
      $ mkAbility a 1
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage EthelRogersMaidenNameMarsh where
  runMessage msg e@(EthelRogersMaidenNameMarsh attrs) = runQueueT $ case msg of
    -- "Add 1 offering to each copy of Antediluvian Sigils."
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach antediluvianSigils $ placeTokensOn (attrs.ability 1) #offering 1
      pure e
    _ -> EthelRogersMaidenNameMarsh <$> liftRunMessage msg attrs
