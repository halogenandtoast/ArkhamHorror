module Arkham.Homebrew.DarkMatter.Agendas.TheThingFromEarth (theThingFromEarth) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Window (assetLeavingPlay)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scanTopOfScanningDeck, scenarioI18n)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Homebrew.DarkMatter.MotionScanning
import Arkham.Message.Lifted.Log

newtype TheThingFromEarth = TheThingFromEarth AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThingFromEarth :: AgendaCard TheThingFromEarth
theThingFromEarth = agenda (2, A) TheThingFromEarth Cards.theThingFromEarth (Static 4)

instance HasModifiersFor TheThingFromEarth where
  getModifiersFor (TheThingFromEarth a) = motionScanModifiers a

instance HasAbilities TheThingFromEarth where
  getAbilities (TheThingFromEarth a) = motionScanAbilities a

instance RunMessage TheThingFromEarth where
  runMessage msg a@(TheThingFromEarth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scanTopOfScanningDeck iid (attrs.ability 1)
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (assetLeavingPlay -> aid) _ -> do
      push $ RemoveFromGame (AssetTarget aid)
      pure a
    {- Agenda 2b, "The Lost":

    "In player order, each investigator must test [willpower] (3). In your
    Campaign Log, for each investigator who fails, record that (their
    investigator name) has been corrupted by the Earth."

    'eachInvestigator' walks the investigators in player order, and
    'advanceAgendaDeckAfterSkillTest' holds the advancement until the last test
    has resolved. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      scenarioI18n "inTheShadowOfEarth" $ scope "agenda2b" do
        flavor $ setTitle "title" >> p "body"
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid attrs iid #willpower (Fixed 3)
      advanceAgendaDeckAfterSkillTest attrs
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      recordForInvestigator iid HasBeenCorruptedByTheEarth
      pure a
    _ -> TheThingFromEarth <$> liftRunMessage msg attrs
