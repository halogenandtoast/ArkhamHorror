module Arkham.Treachery.Cards.SpiresOfCarcosa (spiresOfCarcosa, SpiresOfCarcosa (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SpiresOfCarcosa = SpiresOfCarcosa TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiresOfCarcosa :: TreacheryCard SpiresOfCarcosa
spiresOfCarcosa = treachery SpiresOfCarcosa Cards.spiresOfCarcosa

instance HasAbilities SpiresOfCarcosa where
  getAbilities (SpiresOfCarcosa a) =
    [ investigateAbility a 1 mempty OnSameLocation
    ]
      <> case a.attached of
        Just (LocationTarget lid) ->
          [restrictedAbility a 2 (exists $ LocationWithId lid <> LocationWithoutDoom) Anytime]
        _ -> []

instance RunMessage SpiresOfCarcosa where
  runMessage msg t@(SpiresOfCarcosa attrs) = runQueueT $ case msg of
    Revelation iid source | isSource attrs source -> do
      withLocationOf iid \lid -> do
        attachTreachery attrs lid
        placeDoom attrs lid 2
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ mkInvestigate iid (attrs.ability 1) <&> setTarget attrs
      pure t
    Successful (Action.Investigate, _) _ _ (isTarget attrs -> True) _ -> do
      case attrs.attached of
        Just location -> removeDoom (attrs.ability 1) location 1
        Nothing -> error "must be attached to location to trigger ability"
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> SpiresOfCarcosa <$> liftRunMessage msg attrs
