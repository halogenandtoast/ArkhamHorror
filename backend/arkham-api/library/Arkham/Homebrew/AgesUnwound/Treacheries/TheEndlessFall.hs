module Arkham.Homebrew.AgesUnwound.Treacheries.TheEndlessFall (theEndlessFall) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Homebrew.AgesUnwound.Traits (pattern Adrift)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Treachery.Import.Lifted

newtype TheEndlessFall = TheEndlessFall TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEndlessFall :: TreacheryCard TheEndlessFall
theEndlessFall = treachery TheEndlessFall Cards.theEndlessFall

{- | "while resolving this ability" -- true from the Revelation until the
@After (Revelation …)@ that ends it.
-}
resolvingRevelation :: TreacheryAttrs -> Bool
resolvingRevelation a = toResultDefault False a.meta

instance RunMessage TheEndlessFall where
  runMessage msg t@(TheEndlessFall attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mlid <- selectOne $ locationWithInvestigator iid <> LocationWithTrait Adrift
      case mlid of
        Nothing -> gainSurge attrs
        Just lid -> triggerEndOfTurnForced iid lid
      pure . TheEndlessFall $ setMeta True attrs
    {- "If you fail a skill test by 2 or more while resolving this ability":
    /any/ skill test, not just the location ability this triggered -- but only
    inside the revelation.

    The flag is what bounds it. The card is discarded the instant the revelation
    ends, and a discarded treachery still receives messages, so on the source
    matcher alone the copy sitting in the discard pile went on shoving the
    investigator clockwise every later time they failed a location's end-of-turn
    test by 2 or more. -}
    FailedThisSkillTestBy iid _ n
      | n >= 2
      , resolvingRevelation attrs -> do
          atAdrift <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Adrift
          when atAdrift do
            mlid <- selectOne $ locationWithInvestigator iid
            for_ mlid $ getClockwise 1 >=> traverse_ \next -> do
              moveTo attrs iid next
              triggerEndOfTurnForced iid next
          pure t
    {- Closing the window. Handed on to the runner, which is what discards the
    card from 'Limbo' here -- swallowing it would leave the treachery in play. -}
    After (Revelation _ (isSource attrs -> True)) ->
      TheEndlessFall . setMeta False <$> liftRunMessage msg attrs
    _ -> TheEndlessFall <$> liftRunMessage msg attrs
