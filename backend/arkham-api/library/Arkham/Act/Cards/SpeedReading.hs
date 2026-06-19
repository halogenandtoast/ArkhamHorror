module Arkham.Act.Cards.SpeedReading (speedReading) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.ReadOrDie.Helpers

newtype SpeedReading = SpeedReading ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

speedReading :: ActCard SpeedReading
speedReading = act (1, A) SpeedReading Cards.speedReading Nothing

-- "Ignore all Objectives on locations." The only location Objectives in this
-- scenario are on Dormitories (ability 1) and Faculty Offices (ability 2).
instance HasModifiersFor SpeedReading where
  getModifiersFor (SpeedReading a) = do
    dormitories <- select $ locationIs Locations.dormitories
    facultyOffices <- select $ locationIs Locations.facultyOfficesTheNightIsStillYoung
    let objectives =
          [AbilityIs (toSource l) 1 | l <- dormitories]
            <> [AbilityIs (toSource l) 2 | l <- facultyOffices]
    unless (null objectives) do
      modifySelect a Anyone [CannotTriggerAbilityMatching (AbilityOneOf objectives)]

instance HasAbilities SpeedReading where
  getAbilities = actAbilities1 \a ->
    scenarioI18n
      $ withI18nTooltip "speedReading.takeTome"
      $ restricted
        a
        1
        (youExist daisyWalker <> exists (YourLocation <> LocationWithCardsUnderneath (HasCard IsPlayerCard)))
      $ FastAbility (GroupClueCost (PerPlayer 1) YourLocation)

instance RunMessage SpeedReading where
  runMessage msg a@(SpeedReading attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        underneath <- fieldMap LocationCardsUnderneath (filterCards IsPlayerCard) lid
        for_ (nonEmpty underneath) \cards -> do
          card <- setFacedown False =<< sample cards
          obtainCard card
          push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> SpeedReading <$> liftRunMessage msg attrs
