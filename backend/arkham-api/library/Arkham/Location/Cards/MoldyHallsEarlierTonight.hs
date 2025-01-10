module Arkham.Location.Cards.MoldyHallsEarlierTonight (moldyHallsEarlierTonight) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Card
import Arkham.GameValue
import Arkham.Investigator.Types (Field (InvestigatorDiscard, InvestigatorName))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Name qualified as Name
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype MoldyHallsEarlierTonight = MoldyHallsEarlierTonight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moldyHallsEarlierTonight :: LocationCard MoldyHallsEarlierTonight
moldyHallsEarlierTonight =
  location MoldyHallsEarlierTonight Cards.moldyHallsEarlierTonight 2 (Static 0)

instance HasAbilities MoldyHallsEarlierTonight where
  getAbilities (MoldyHallsEarlierTonight a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        (Here <> exists (DiscardWith AnyCards <> can.have.cards.leaveDiscard <> at_ (be a)))
        actionAbility

instance RunMessage MoldyHallsEarlierTonight where
  runMessage msg l@(MoldyHallsEarlierTonight attrs) = runQueueT $ case msg of
    Msg.RevealLocation _ (is attrs -> True) -> do
      MoldyHallsEarlierTonight <$> liftRunMessage msg (attrs & labelL .~ "moldyHallsEarlierTonight")
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iids <- select $ at_ (be attrs) <> DiscardWith AnyCards <> can.have.cards.leaveDiscard

      for_ iids \iid -> do
        name <- field InvestigatorName iid
        discards <- fieldMap InvestigatorDiscard (map toCard) iid
        chooseOneM iid do
          labeled "Do not request aid from your past self" nothing
          labeled "Request aid from your past self" do
            focusCards discards do
              chooseTargetM iid discards (returnToHand iid)
            remember $ MeddledWithThePast $ Name.labeled name iid
      pure l
    _ -> MoldyHallsEarlierTonight <$> liftRunMessage msg attrs
