module Arkham.Act.Cards.EnteringTheUnderworldV1 (EnteringTheUnderworldV1 (..), enteringTheUnderworldV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet (EncounterSet (Ghouls, Nightgaunts, StrikingFear, TerrorOfTheVale))
import Arkham.Enemy.Types (Field (EnemyClues))
import Arkham.Helpers.Query
import Arkham.Helpers.Window (getEnemy)
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Vale))

newtype EnteringTheUnderworldV1 = EnteringTheUnderworldV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enteringTheUnderworldV1 :: ActCard EnteringTheUnderworldV1
enteringTheUnderworldV1 = act (1, A) EnteringTheUnderworldV1 Cards.enteringTheUnderworldV1 Nothing

instance HasAbilities EnteringTheUnderworldV1 where
  getAbilities (EnteringTheUnderworldV1 x) =
    -- Use EnemyDefeated (resolves before the enemy leaves play) rather than
    -- IfEnemyDefeated: these enemies have victory and are removed to the
    -- victory display on defeat, clearing their clues before IfEnemyDefeated
    -- would resolve.
    [mkAbility x 1 $ forced $ EnemyDefeated #after Anyone ByAny $ EnemyWithClues $ atLeast 1]

instance RunMessage EnteringTheUnderworldV1 where
  runMessage msg a@(EnteringTheUnderworldV1 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getEnemy -> eid) _ -> do
      clues <- field EnemyClues eid
      when (clues > 0) $ push $ GainClues iid (attrs.ability 1) clues
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      traverse_ placeLocation_ =<< getSetAsideCardsMatching (#location <> withTrait Vale)
      push $ RemoveAllCopiesOfEncounterCardFromGame (fromSets [Ghouls, StrikingFear])
      encounterSets <- getSetAsideCardsMatching (fromSets [TerrorOfTheVale, Nightgaunts])
      pushAll [ShuffleCardsIntoDeck Deck.EncounterDeck encounterSets, ShuffleEncounterDiscardBackIn]
      advanceActDeck attrs
      pure a
    _ -> EnteringTheUnderworldV1 <$> liftRunMessage msg attrs
