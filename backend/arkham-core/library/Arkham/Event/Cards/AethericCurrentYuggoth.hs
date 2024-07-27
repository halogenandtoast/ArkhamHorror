module Arkham.Event.Cards.AethericCurrentYuggoth (
  aethericCurrentYuggoth,
  AethericCurrentYuggoth (..),
)
where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Token

newtype AethericCurrentYuggoth = AethericCurrentYuggoth EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aethericCurrentYuggoth :: EventCard AethericCurrentYuggoth
aethericCurrentYuggoth = event AethericCurrentYuggoth Cards.aethericCurrentYuggoth

instance RunMessage AethericCurrentYuggoth where
  runMessage msg e@(AethericCurrentYuggoth attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <- select $ assetControlledBy iid
      for_ assets \asset -> do
        clues <- field AssetClues asset
        push $ MoveTokens (toSource attrs) (toSource asset) (toTarget iid) Clue clues

      sid <- getRandom
      chooseFightEnemyWithSkillChoice sid iid attrs [#combat, #intellect]
      drawCardsIfCan iid attrs 1
      fluxStabilizer <- selectJust $ assetIs Assets.fluxStabilizerActive
      push $ Flip iid (toSource attrs) (toTarget fluxStabilizer)
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      whenJustM (getSkillTestTarget) \case
        EnemyTarget eid -> do
          isNonElite <- eid <=~> NonEliteEnemy
          when isNonElite do
            locations <- select $ oneOf [LocationCanBeEnteredBy eid, locationWithEnemy eid]
            chooseOneM iid do
              labeled "Exhaust Enemy and move it" do
                push $ Exhaust (toTarget eid)
                chooseOneM iid do
                  for_ locations \lid -> targeting lid $ push $ EnemyMove eid lid
              labeled "Do not exhaust enemy" nothing
        _ -> pure ()
      pure e
    _ -> AethericCurrentYuggoth <$> liftRunMessage msg attrs
