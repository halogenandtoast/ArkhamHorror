module Arkham.Event.Cards.AethericCurrentYoth (
  aethericCurrentYoth,
  AethericCurrentYoth (..),
)
where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Token

newtype AethericCurrentYoth = AethericCurrentYoth EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aethericCurrentYoth :: EventCard AethericCurrentYoth
aethericCurrentYoth = event AethericCurrentYoth Cards.aethericCurrentYoth

instance RunMessage AethericCurrentYoth where
  runMessage msg e@(AethericCurrentYoth attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <- select $ assetControlledBy iid
      for_ assets \asset -> do
        clues <- field AssetClues asset
        push $ MoveTokens (toSource attrs) (toSource asset) (toTarget iid) Clue clues

      sid <- getRandom
      chooseEvadeEnemyWithSkillChoice sid iid attrs [#agility, #intellect]
      drawCardsIfCan iid attrs 1
      fluxStabilizer <- selectJust $ assetIs Assets.fluxStabilizerActive
      push $ Flip iid (toSource attrs) (toTarget fluxStabilizer)
      pure e
    PassedThisSkillTest _iid (isSource attrs -> True) -> do
      whenJustM getSkillTestTarget \case
        EnemyTarget eid -> whenM (eid <=~> NonEliteEnemy) $ shuffleIntoDeck Deck.EncounterDeck eid
        _ -> pure ()
      pure e
    _ -> AethericCurrentYoth <$> liftRunMessage msg attrs
