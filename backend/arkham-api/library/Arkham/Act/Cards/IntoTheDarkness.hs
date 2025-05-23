module Arkham.Act.Cards.IntoTheDarkness (intoTheDarkness) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query
import Arkham.Matcher

newtype IntoTheDarkness = IntoTheDarkness ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheDarkness :: ActCard IntoTheDarkness
intoTheDarkness = act (2, A) IntoTheDarkness Cards.intoTheDarkness Nothing

instance HasAbilities IntoTheDarkness where
  getAbilities (IntoTheDarkness attrs) =
    [mkAbility attrs 1 $ Objective $ forced $ Enters #after Anyone "Ritual Site" | onSide A attrs]

instance RunMessage IntoTheDarkness where
  runMessage msg a@(IntoTheDarkness attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      playerCount <- getPlayerCount
      shuffleEncounterDiscardBackIn
      lead <- getLead
      discardUntilFirst lead attrs Deck.EncounterDeck #enemy
      when (playerCount > 3) do
        discardUntilFirst lead attrs Deck.EncounterDeck #enemy
      advanceActDeck attrs
      pure a
    RequestedEncounterCard (isSource attrs -> True) _ (Just card) -> do
      spawnEnemyAt_ card =<< getJustLocationByName "Ritual Site"
      pure a
    _ -> IntoTheDarkness <$> liftRunMessage msg attrs
