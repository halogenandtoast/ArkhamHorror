module Arkham.Treachery.Cards.TheBlackWind (theBlackWind) where

import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheBlackWind = TheBlackWind TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackWind :: TreacheryCard TheBlackWind
theBlackWind = treachery TheBlackWind Cards.theBlackWind

instance RunMessage TheBlackWind where
  runMessage msg t@(TheBlackWind attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      campaignI18n $ chooseOneM iid do
        labeled' "theBlackWind.addStrength" do
          addStrengthOfTheAbyss 1
          addToVictory iid attrs
        labeled' "theBlackWind.discard" do
          toDiscardBy iid attrs attrs
          roundModifiers attrs iid [SkillModifier sk (-1) | sk <- [minBound..]]
          drawEncounterCard iid attrs
      pure t
    _ -> TheBlackWind <$> liftRunMessage msg attrs
