module Arkham.Homebrew.AgesUnwound.Treacheries.TheEndlessFall (theEndlessFall) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Homebrew.AgesUnwound.Traits (pattern Adrift)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Source
import Arkham.Treachery.Import.Lifted

newtype TheEndlessFall = TheEndlessFall TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEndlessFall :: TreacheryCard TheEndlessFall
theEndlessFall = treachery TheEndlessFall Cards.theEndlessFall

{- | "Revelation - If you are not at an [[Adrift]] location, The Endless Fall
gains surge. Otherwise, trigger the forced ability on your location as if it
were the end of your turn. If you fail a skill test by 2 or more while resolving
this ability, move to the location in the clockwise direction, then repeat this
process."

The chain is bounded by the ring: each lap moves one position clockwise, and the
failure that continues it has to come from the location's own Forced. Both /An
Earth Long Dead/ printings trigger on /entry/ rather than at end of turn, so
landing on one ends the chain -- there is no end-of-turn Forced to trigger.
-}
instance RunMessage TheEndlessFall where
  runMessage msg t@(TheEndlessFall attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mlid <- selectOne $ locationWithInvestigator iid <> LocationWithTrait Adrift
      case mlid of
        Nothing -> gainSurge attrs
        Just lid -> triggerEndOfTurnForced iid lid
      pure t
    {- The location's Forced is a different source, so this watches every failed
    test the investigator makes while it is resolving. The treachery is still in
    play only for that stretch, which is what scopes it. -}
    FailedThisSkillTestBy iid (AbilitySource (LocationSource _) _) n | n >= 2 -> do
      atAdrift <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Adrift
      when atAdrift do
        mlid <- selectOne $ locationWithInvestigator iid
        for_ mlid \lid ->
          getClockwise 1 lid >>= traverse_ \next -> do
            moveTo attrs iid next
            triggerEndOfTurnForced iid next
      pure t
    _ -> TheEndlessFall <$> liftRunMessage msg attrs
