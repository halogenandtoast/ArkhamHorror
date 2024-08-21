module Arkham.Treachery.Cards.PsychopompsSong (psychopompsSong, PsychopompsSong (..)) where

import Arkham.Ability
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers qualified as Msg
import Arkham.Treachery.Import.Lifted

newtype PsychopompsSong = PsychopompsSong TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychopompsSong :: TreacheryCard PsychopompsSong
psychopompsSong = treachery PsychopompsSong Cards.psychopompsSong

instance HasAbilities PsychopompsSong where
  getAbilities (PsychopompsSong attrs) =
    [ restrictedAbility attrs 1 (InThreatAreaOf You)
        $ forced
        $ InvestigatorWouldTakeDamage #when You AnySource AnyDamageType
    ]

instance RunMessage PsychopompsSong where
  runMessage msg t@(PsychopompsSong attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- getInvestigators
      chooseOrRunOne iid $ targetLabels investigators $ only . Msg.placeInThreatArea attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      dealAdditionalDamage iid 2 [Msg.toDiscardBy iid (attrs.ability 1) attrs]
      pure t
    _ -> PsychopompsSong <$> liftRunMessage msg attrs
