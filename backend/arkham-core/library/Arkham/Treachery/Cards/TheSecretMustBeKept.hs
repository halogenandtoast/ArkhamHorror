module Arkham.Treachery.Cards.TheSecretMustBeKept (
  theSecretMustBeKept,
  TheSecretMustBeKept (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TheSecretMustBeKept = TheSecretMustBeKept TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecretMustBeKept :: TreacheryCard TheSecretMustBeKept
theSecretMustBeKept = treachery TheSecretMustBeKept Cards.theSecretMustBeKept

instance RunMessage TheSecretMustBeKept where
  runMessage msg t@(TheSecretMustBeKept attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      -- max 3 decks so we subtract the number of decks in play from 3
      push
        $ RevelationSkillTest
          iid
          source
          SkillWillpower
          (SumDifficulty [Fixed 3, SubtractDifficulty (Fixed 3) ActsInPlayDifficulty])
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        deckCount <- getActDecksInPlayCount
        let n = 3 - deckCount
        push
          $ InvestigatorAssignDamage
            iid
            (toSource attrs)
            DamageAny
            (1 + n)
            (1 + n)
        pure t
    _ -> TheSecretMustBeKept <$> runMessage msg attrs
