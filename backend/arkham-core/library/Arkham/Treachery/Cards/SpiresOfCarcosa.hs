module Arkham.Treachery.Cards.SpiresOfCarcosa (
  spiresOfCarcosa,
  SpiresOfCarcosa (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SpiresOfCarcosa = SpiresOfCarcosa TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

spiresOfCarcosa :: TreacheryCard SpiresOfCarcosa
spiresOfCarcosa = treachery SpiresOfCarcosa Cards.spiresOfCarcosa

instance HasAbilities SpiresOfCarcosa where
  getAbilities (SpiresOfCarcosa a) =
    [ investigateAbility a 1 mempty OnSameLocation
    ]
      <> case treacheryAttachedTarget a of
        Just (LocationTarget lid) ->
          [ restrictedAbility a 2 (exists $ LocationWithId lid <> LocationWithoutDoom)
              $ SilentForcedAbility AnyWindow
          ]
        _ -> []

instance RunMessage SpiresOfCarcosa where
  runMessage msg t@(SpiresOfCarcosa attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- fieldJust InvestigatorLocation iid
      pushAll
        [AttachTreachery (toId attrs) (toTarget lid), PlaceDoom (toSource attrs) (toTarget lid) 2]
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ mkInvestigate iid (toAbilitySource attrs 1) <&> setTarget attrs
      pure t
    Successful (Action.Investigate, _) _ _ target _ | isTarget attrs target -> do
      case treacheryAttachedTarget attrs of
        Just location -> push (RemoveDoom (toAbilitySource attrs 1) location 1)
        Nothing -> error "must be attached to location to trigger ability"
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 2) attrs
      pure t
    _ -> SpiresOfCarcosa <$> runMessage msg attrs
