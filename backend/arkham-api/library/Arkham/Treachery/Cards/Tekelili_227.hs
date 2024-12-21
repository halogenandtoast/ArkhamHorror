module Arkham.Treachery.Cards.Tekelili_227 (tekelili_227, tekelili_227Effect, Tekelili_227 (..)) where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers (ModifierType (..), hasModifier)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Tekelili_227 = Tekelili_227 TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tekelili_227 :: TreacheryCard Tekelili_227
tekelili_227 = treachery Tekelili_227 Cards.tekelili_227

instance RunMessage Tekelili_227 where
  runMessage msg t@(Tekelili_227 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      isTurn <- iid <=~> TurnInvestigator

      n <- ifM_ (hasModifier (toCard attrs) ResolveEffectsAgain) 2 1
      repeated n
        $ if isTurn
          then loseActions iid attrs 1
          else createCardEffect Cards.tekelili_227 Nothing attrs iid

      resolveTekelili iid attrs
      pure t
    _ -> Tekelili_227 <$> liftRunMessage msg attrs

newtype Tekelili_227Effect = Tekelili_227Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tekelili_227Effect :: EffectArgs -> Tekelili_227Effect
tekelili_227Effect = cardEffect Tekelili_227Effect Cards.tekelili_227

instance RunMessage Tekelili_227Effect where
  runMessage msg e@(Tekelili_227Effect attrs) = runQueueT $ case msg of
    BeginTurn iid | isTarget iid attrs.target -> do
      loseActions iid attrs.source 1
      disableReturn e
    _ -> Tekelili_227Effect <$> liftRunMessage msg attrs
