module Arkham.Agenda.Cards.OtherworldlyHorror (otherworldlyHorror) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.WithoutATrace.Helpers
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype OtherworldlyHorror = OtherworldlyHorror AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyHorror :: AgendaCard OtherworldlyHorror
otherworldlyHorror = agenda (1, A) OtherworldlyHorror Cards.otherworldlyHorror (Static 5)

instance HasModifiersFor OtherworldlyHorror where
  getModifiersFor (OtherworldlyHorror a) = do
    modifySelect a UnrevealedLocation [AdditionalCostToEnter $ GroupClueCost (PerPlayer 1) YourLocation]

instance HasAbilities OtherworldlyHorror where
  getAbilities (OtherworldlyHorror a) = [mkAbility a 1 $ freeReaction (ScenarioEvent #after (Just You) "exposedAdjacentLocation")]

getExposedLocation :: [Window] -> LocationId
getExposedLocation = \case
  [] -> error "missing exposed location"
  ((windowType -> Window.ScenarioEvent "exposedAdjacentLocation" _ value) : _) -> toResult value
  (_ : xs) -> getExposedLocation xs

instance RunMessage OtherworldlyHorror where
  runMessage msg a@(OtherworldlyHorror attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      moveTo (attrs.ability 1) iid (getExposedLocation ws)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleSetAsideIntoEncounterDeck [Enemies.apocalypticPresage, Enemies.protoplasmicReassembler]
      lead <- getLead
      investigators <- getInvestigators
      canHollow <-
        investigators & anyM \iid ->
          selectAny
            $ basic (NonWeakness <> not_ PermanentCard)
            <> oneOf [inHandOf NotForPlay iid, inPlayAreaOf iid]

      anyConcealed <- selectAny ConcealedCardAny

      chooseNM lead 2 $ scenarioI18n do
        labeled' "otherworldlyHorror.takeDamageAndHorror" do
          eachInvestigator \iid -> assignDamageAndHorror iid attrs 1 1
        labeledValidate' canHollow "otherworldlyHorror.hollows" do
          eachInvestigator \iid -> do
            cards <-
              select
                $ basic (NonWeakness <> not_ PermanentCard)
                <> oneOf [inHandOf NotForPlay iid, inPlayAreaOf iid]
            focusCards cards $ chooseTargetM iid cards $ hollow iid
        labeledValidate' anyConcealed "otherworldlyHorror.shuffleAllConcealed" do
          scenarioSpecific_ "shuffleAllConcealed"
      advanceAgendaDeck attrs
      pure a
    _ -> OtherworldlyHorror <$> liftRunMessage msg attrs
