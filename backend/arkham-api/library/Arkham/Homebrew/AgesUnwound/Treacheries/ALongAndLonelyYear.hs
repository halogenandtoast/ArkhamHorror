module Arkham.Homebrew.AgesUnwound.Treacheries.ALongAndLonelyYear (aLongAndLonelyYear) where

import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Treachery.Import.Lifted

newtype ALongAndLonelyYear = ALongAndLonelyYear TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aLongAndLonelyYear :: TreacheryCard ALongAndLonelyYear
aLongAndLonelyYear = treachery ALongAndLonelyYear Cards.aLongAndLonelyYear

{- | "__Revelation__ - Test [willpower] (X), where X is 1 more than the current
agenda number. If you fail, either take 1 horror for each point you failed by,
or move to Arkham, Massachusetts."

One choice for the whole failure, not one per point -- so no 'doStep' loop. The
move is offered only when Arkham, Massachusetts is in play and reachable; with
nowhere to go the horror is the only branch, which is what "either ... or" gives
an investigator who cannot take one side.
-}
instance RunMessage ALongAndLonelyYear where
  runMessage msg t@(ALongAndLonelyYear attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      n <- getCurrentAgendaStep
      revelationSkillTest sid iid attrs #willpower (Fixed $ n + 1)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      arkham <- selectOne $ locationIs Locations.arkhamMassachusetts_111
      chooseOrRunOneM iid $ campaignI18n do
        unscoped $ countVar n $ labeled "takeHorror" $ assignHorror iid attrs n
        for_ arkham $ labeled "aLongAndLonelyYear.moveToArkham" . moveTo attrs iid
      pure t
    _ -> ALongAndLonelyYear <$> liftRunMessage msg attrs
