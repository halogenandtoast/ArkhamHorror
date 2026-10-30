module Arkham.Homebrew.NightOfVespers.Enemies.SkulkingHooligan (skulkingHooligan) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.NightOfVespers.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.NightOfVespers.Helpers (scenarioI18n)
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype SkulkingHooligan = SkulkingHooligan EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skulkingHooligan :: EnemyCard SkulkingHooligan
skulkingHooligan =
  enemy SkulkingHooligan Cards.skulkingHooligan
    & setSpawnAt (FarthestLocationFromAll RevealedLocation)

instance HasAbilities SkulkingHooligan where
  getAbilities (SkulkingHooligan a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #when You (be a)

instance RunMessage SkulkingHooligan where
  runMessage msg e@(SkulkingHooligan attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clues <- field InvestigatorClues iid
      chooseOneM iid $ scenarioI18n "theEndOfSeasons" do
        labeled "skulkingHooligan.returnClues" do
          removeClues (attrs.ability 1) iid ((clues + 1) `div` 2)
          withLocationOf iid $ placeCluesUpToClueValue (attrs.ability 1)
        labeled "skulkingHooligan.placeDoom" do
          placeDoomOnAgendaAndCheckAdvanceBy (attrs.ability 1) 1
      pure e
    _ -> SkulkingHooligan <$> liftRunMessage msg attrs
