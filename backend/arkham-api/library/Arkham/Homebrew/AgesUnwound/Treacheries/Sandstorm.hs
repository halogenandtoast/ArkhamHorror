module Arkham.Homebrew.AgesUnwound.Treacheries.Sandstorm (sandstorm) where

import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Helpers.Modifiers (ModifierType (ShroudModifier), modifySelectWhen)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Traits (pattern Adrift)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype Sandstorm = Sandstorm TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sandstorm :: TreacheryCard Sandstorm
sandstorm = treachery Sandstorm Cards.sandstorm

-- | "Attached location gets +1 shroud for each doom in play (min 1)."
instance HasModifiersFor Sandstorm where
  getModifiersFor (Sandstorm a) = do
    doom <- getDoomCount
    modifySelectWhen a (isJust a.attached) (locationWithTreachery a) [ShroudModifier (max 1 doom)]

{- | "Revelation - Attach to the revealed [[Adrift]] location with the most clues
and without a copy of Sandstorm attached."
-}
instance RunMessage Sandstorm where
  runMessage msg t@(Sandstorm attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      candidates <-
        select
          $ LocationWithMostClues
            ( RevealedLocation
                <> LocationWithTrait Adrift
                <> not_ (LocationWithTreachery $ treacheryIs Cards.sandstorm)
            )
      case candidates of
        [] -> gainSurge attrs
        lid : _ -> attachTreachery attrs lid
      pure t
    _ -> Sandstorm <$> liftRunMessage msg attrs
