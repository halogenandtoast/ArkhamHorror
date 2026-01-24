module Arkham.Agenda.Cards.UndergroundSurvey (undergroundSurvey) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveAllTo)
import Arkham.Token
import Arkham.Trait (Trait (Power))

newtype UndergroundSurvey = UndergroundSurvey AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundSurvey :: AgendaCard UndergroundSurvey
undergroundSurvey =
  agendaWith (1, A) UndergroundSurvey Cards.undergroundSurvey (Static 5)
    $ removeDoomMatchersL
    %~ (\rdm -> rdm {removeDoomAgendas = NotAgenda AnyAgenda})

instance HasAbilities UndergroundSurvey where
  getAbilities (UndergroundSurvey a) =
    [ mkAbility a 1 $ forced $ AgendaAdvances #when (AgendaWithDoom $ atLeast 1)
    , restricted a 2 (youExist $ at_ $ LocationWithToken Scrap)
        $ actionAbilityWithCost
        $ DrawEncounterCardsCost 1
    ]

instance RunMessage UndergroundSurvey where
  runMessage msg a@(UndergroundSurvey attrs) = runQueueT $ case msg of
    DrewCards _ drew | isAbilitySource attrs 2 drew.source -> do
      let power = any (`cardMatch` card_ (#treachery <> withTrait Power)) drew.cards
      pure $ UndergroundSurvey $ attrs & setMeta power
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      withLocationOf iid \loc -> do
        moveTokens (attrs.ability 2) loc ScenarioTarget Scrap 1
      when (getMetaDefault False attrs) do
        gainResources iid (attrs.ability 2) 5
      pure $ UndergroundSurvey $ attrs & setMeta False
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        assignDamageTo attrs 2 iid
        selectEach (assetControlledBy iid <> #ally) \asset ->
          dealAssetDirectDamage asset attrs 1
        loseAllClues iid attrs
      controlStation <- selectJust $ locationIs Locations.controlStation
      reveal controlStation
      moveAllTo attrs controlStation
      advanceToActA attrs Acts.theUndergroundMaze
      advanceAgendaDeck attrs
      placeDoomOnAgenda attrs.doom
      selectEach (assetIs Assets.drRosaMarquezBestInHerField) \asset -> do
        clearAbilityUse $ AbilityRef (toSource asset) 1
      pure a
    _ -> UndergroundSurvey <$> liftRunMessage msg attrs
