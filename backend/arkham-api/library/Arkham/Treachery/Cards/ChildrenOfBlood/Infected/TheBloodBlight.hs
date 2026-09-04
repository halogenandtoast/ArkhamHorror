module Arkham.Treachery.Cards.ChildrenOfBlood.Infected.TheBloodBlight (theBloodBlight) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Infected qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheBloodBlight = TheBloodBlight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBloodBlight :: TreacheryCard TheBloodBlight
theBloodBlight = treachery TheBloodBlight Cards.theBloodBlight

instance HasAbilities TheBloodBlight where
  getAbilities (TheBloodBlight x) =
    [ restricted x 1 (InThreatAreaOf You <> exists (chaosToken_ #blood)) $ forced $ RoundEnds #when
    , restricted x 2 (InThreatAreaOf You <> exists (SealedOnInvestigator You #blood)) actionAbility
    ]

instance RunMessage TheBloodBlight where
  runMessage msg t@(TheBloodBlight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (chaosToken_ #blood) >>= traverse_ (sealChaosToken iid iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      blood <- select $ SealedOnInvestigator (InvestigatorWithId iid) #blood
      chooseOneM iid $ withI18n do
        for_ [1 .. min 3 (length blood)] \n -> countVar n $ labeled "dealDamage" $ doStep n msg
      pure t
    DoStep n (UseThisAbility iid (isSource attrs -> True) 2) -> do
      blood <- select $ SealedOnInvestigator (InvestigatorWithId iid) #blood
      investigators <- select $ colocatedWith iid
      chooseTargetM iid investigators \iid' -> do
        assignDamage iid' (attrs.ability 2) n
        for_ (take n blood) unsealChaosToken
        when (n == 3) $ removeChaosToken #blood
      pure t
    _ -> TheBloodBlight <$> liftRunMessage msg attrs
