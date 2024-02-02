module Arkham.Treachery.Cards.NobodysHome (
  nobodysHome,
  NobodysHome (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (InvestigatorLocation))
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype NobodysHome = NobodysHome TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

nobodysHome :: TreacheryCard NobodysHome
nobodysHome = treachery NobodysHome Cards.nobodysHome

instance HasModifiersFor NobodysHome where
  getModifiersFor (AbilityTarget _ ab) (NobodysHome attrs) = do
    case abilitySource ab of
      LocationSource lid | treacheryOnLocation lid attrs -> do
        if Action.Investigate `elem` abilityActions ab
          then pure $ toModifiers attrs [AdditionalCost (ActionCost 1)]
          else pure []
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities NobodysHome where
  getAbilities (NobodysHome a) = case treacheryAttachedTarget a of
    Just (LocationTarget lid) ->
      [ restrictedAbility
          a
          1
          (exists $ LocationWithId lid <> LocationWithoutClues)
          $ ForcedAbility AnyWindow
      ]
    _ -> []

instance RunMessage NobodysHome where
  runMessage msg t@(NobodysHome attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mLocation <- field InvestigatorLocation iid
      for_ mLocation \lid -> do
        clueless <- fieldP LocationClues (== 0) lid
        pushAll
          $ attachTreachery attrs lid
          : [gainSurge attrs | clueless]
      pure t
    UseCardAbility _iid source 1 _ _ | isSource attrs source -> do
      push $ toDiscard (toAbilitySource attrs 1) attrs
      pure t
    _ -> NobodysHome <$> runMessage msg attrs
