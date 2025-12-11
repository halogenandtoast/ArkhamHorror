module Arkham.Agenda.Cards.OtherworldlySlaughter (otherworldlySlaughter) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as Act
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.WithoutATrace.Helpers
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype OtherworldlySlaughter = OtherworldlySlaughter AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlySlaughter :: AgendaCard OtherworldlySlaughter
otherworldlySlaughter = agenda (3, A) OtherworldlySlaughter Cards.otherworldlySlaughter (Static 6)

instance HasModifiersFor OtherworldlySlaughter where
  getModifiersFor (OtherworldlySlaughter a) = do
    modifySelect a UnrevealedLocation [AdditionalCostToEnter $ GroupClueCost (PerPlayer 1) YourLocation]

instance HasAbilities OtherworldlySlaughter where
  getAbilities (OtherworldlySlaughter a) =
    [ mkAbility a 1 $ freeReaction (ScenarioEvent #after (Just You) "exposedAdjacentLocation")
    , mkAbility a 2
        $ FastAbility
        $ OrCost [DirectHorrorCost (a.ability 2) You 1, DirectDamageCost (a.ability 2) You 1]
    ]

getExposedLocation :: [Window] -> LocationId
getExposedLocation = \case
  [] -> error "missing exposed location"
  ((windowType -> Window.ScenarioEvent "exposedAdjacentLocation" _ value) : _) -> toResult value
  (_ : xs) -> getExposedLocation xs

instance RunMessage OtherworldlySlaughter where
  runMessage msg a@(OtherworldlySlaughter attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      moveTo (attrs.ability 1) iid (getExposedLocation ws)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      cards <- selectMap toId ConcealedCardAny
      chooseTargetM iid cards $ revealConcealed iid (attrs.ability 2)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      mresigned <- selectOne ResignedInvestigator
      eachInvestigator $ push . DrivenInsane
      case mresigned of
        Nothing -> push CheckForRemainingInvestigators
        Just iid -> chooseOneM iid $ scenarioI18n do
          labeled' "otherworldlySlaughter.advanceToAct3b" do
            advanceToAct' attrs 1 Acts.escapingTheOtherworld Act.B
          unscoped $ labeled' "skip" $ push CheckForRemainingInvestigators
      pure a
    _ -> OtherworldlySlaughter <$> liftRunMessage msg attrs
