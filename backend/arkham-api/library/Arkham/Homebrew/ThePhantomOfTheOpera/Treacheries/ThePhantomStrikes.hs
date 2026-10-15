module Arkham.Homebrew.ThePhantomOfTheOpera.Treacheries.ThePhantomStrikes (thePhantomStrikes) where

import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype ThePhantomStrikes = ThePhantomStrikes TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePhantomStrikes :: TreacheryCard ThePhantomStrikes
thePhantomStrikes = treachery ThePhantomStrikes Cards.thePhantomStrikes

instance RunMessage ThePhantomStrikes where
  runMessage msg t@(ThePhantomStrikes attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignDamage iid attrs 2
      when (n >= 3) do
        inShadows <- selectAny $ EnemyWithPlacement InTheShadows
        when inShadows $ withLocationOf iid $ makeDecoyAt iid
      pure t
    PassedThisSkillTestBy iid (isSource attrs -> True) n -> do
      when (n >= 2) $ withLocationOf iid \lid -> do
        concealed <- getConcealedAt NotForExpose lid
        unless (null concealed) $ chooseRevealConcealedAt iid attrs (LocationWithId lid)
      pure t
    _ -> ThePhantomStrikes <$> liftRunMessage msg attrs
