module Arkham.Agenda.Cards.DoomFromBelow (doomFromBelow) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.CityOfTheElderThings.Helpers

newtype DoomFromBelow = DoomFromBelow AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doomFromBelow :: AgendaCard DoomFromBelow
doomFromBelow = agenda (1, A) DoomFromBelow Cards.doomFromBelow (Static 10)

instance HasAbilities DoomFromBelow where
  getAbilities (DoomFromBelow a) =
    scenarioI18n
      [ withI18nTooltip "cityLandscape.look"
        $ playerLimit PerRound
        $ restricted a 1 (DuringTurn You <> exists (UnrevealedLocation <> inYourColumnOrRow))
        $ FastAbility (ClueCost $ Static 1)
      , withI18nTooltip "cityLandscape.move"
        $ playerLimit PerRound
        $ restricted a 2 (DuringTurn You <> exists (CanMoveToLocation You (toSource a) inYourColumnOrRow))
        $ FastAbility (ClueCost $ Static 3)
      ]
   where
    inYourColumnOrRow = columnOrRowOf (LocationWithInvestigator You)

columnOrRowOf :: LocationMatcher -> LocationMatcher
columnOrRowOf inner = oneOf [LocationInRowOf inner, LocationInColumnOf inner]

instance RunMessage DoomFromBelow where
  runMessage msg a@(DoomFromBelow attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ls <- select $ UnrevealedLocation <> columnOrRowOf (locationWithInvestigator iid)
      chooseTargetM iid ls $ lookAtRevealed iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      ls <-
        select $ CanMoveToLocation (be iid) (attrs.ability 2) (columnOrRowOf $ locationWithInvestigator iid)
      chooseTargetM iid ls $ moveTo (attrs.ability 1) iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R2
      pure a
    _ -> DoomFromBelow <$> liftRunMessage msg attrs
