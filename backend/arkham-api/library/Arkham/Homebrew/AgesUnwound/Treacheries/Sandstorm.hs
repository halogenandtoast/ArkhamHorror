module Arkham.Homebrew.AgesUnwound.Treacheries.Sandstorm (sandstorm) where

import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Helpers.Modifiers (ModifierType (ShroudModifier), modifySelectWhen)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Traits (pattern Adrift)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype Sandstorm = Sandstorm TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sandstorm :: TreacheryCard Sandstorm
sandstorm = treachery Sandstorm Cards.sandstorm

instance HasModifiersFor Sandstorm where
  getModifiersFor (Sandstorm a) = do
    doom <- getDoomCount
    modifySelectWhen a (isJust a.attached) (locationWithTreachery a) [ShroudModifier (max 1 doom)]

instance RunMessage Sandstorm where
  runMessage msg t@(Sandstorm attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      candidates <-
        select
          $ LocationWithMostClues
          $ RevealedLocation
          <> LocationWithTrait Adrift
          <> not_ (LocationWithTreachery $ treacheryIs Cards.sandstorm)
      case candidates of
        [] -> gainSurge attrs
        lids -> chooseTargetM iid lids $ attachTreachery attrs
      pure t
    _ -> Sandstorm <$> liftRunMessage msg attrs
