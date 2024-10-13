module Arkham.Location.Cards.GrandRue (grandRue, GrandRue (..)) where

import Arkham.Ability
import Arkham.Agenda.Types (Field (AgendaDoom))
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GrandRue = GrandRue LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandRue :: LocationCard GrandRue
grandRue = location GrandRue Cards.grandRue 1 (PerPlayer 1)

instance HasAbilities GrandRue where
  getAbilities (GrandRue a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be a)
      $ SuccessResult (atMost 1)

instance RunMessage GrandRue where
  runMessage msg l@(GrandRue attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      maxDoom <- fieldMax AgendaDoom AnyAgenda
      agendas <- select $ AgendaWithDoom $ EqualTo $ Static maxDoom
      chooseOrRunOneM iid do
        targets agendas \agenda -> placeDoom (attrs.ability 1) agenda 1
      pure l
    _ -> GrandRue <$> liftRunMessage msg attrs
