{-# LANGUAGE MultiWayIf #-}

module Arkham.Treachery.Cards.ForcedIntoHiding (forcedIntoHiding, ForcedIntoHiding (..)) where

import Arkham.Classes
import Arkham.Message
import Arkham.Prelude
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ForcedIntoHiding = ForcedIntoHiding TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forcedIntoHiding :: TreacheryCard ForcedIntoHiding
forcedIntoHiding = treachery ForcedIntoHiding Cards.forcedIntoHiding

instance RunMessage ForcedIntoHiding where
  runMessage msg t@(ForcedIntoHiding attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      x <- getAlarmLevel iid
      push $ revelationSkillTest iid attrs #willpower x
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      let
        lostActions =
          if
            | n >= 5 -> 3
            | n >= 3 -> 2
            | otherwise -> 1
      push $ LoseActions iid (toSource attrs) lostActions
      pure t
    _ -> ForcedIntoHiding <$> runMessage msg attrs
