module Arkham.Treachery.Cards.NobodysHome
  ( nobodysHome
  , NobodysHome(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types ( Field (InvestigatorLocation) )
import Arkham.Location.Types ( Field (LocationClues) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype NobodysHome = NobodysHome TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nobodysHome :: TreacheryCard NobodysHome
nobodysHome = treachery NobodysHome Cards.nobodysHome

instance HasModifiersFor NobodysHome where
  getModifiersFor (AbilityTarget _ ab) (NobodysHome attrs) = do
    case abilitySource ab of
      LocationSource lid | treacheryOnLocation lid attrs -> do
        case abilityAction ab of
          Just Action.Investigate ->
            pure $ toModifiers attrs [AdditionalCost (ActionCost 1)]
          _ -> pure []
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities NobodysHome where
  getAbilities (NobodysHome a) = case treacheryAttachedTarget a of
    Just (LocationTarget lid) ->
      [ restrictedAbility
            a
            1
            (LocationExists $ LocationWithId lid <> LocationWithoutClues)
          $ ForcedAbility AnyWindow
      ]
    _ -> []

instance RunMessage NobodysHome where
  runMessage msg t@(NobodysHome attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mLocation <- field InvestigatorLocation iid
      for_ mLocation $ \lid -> do
        clueless <- fieldP LocationClues (== 0) lid
        pushAll
          $ AttachTreachery (toId attrs) (LocationTarget lid)
          : [ Surge iid source | clueless ]
      pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      t <$ push (Discard (toAbilitySource attrs 1) $ toTarget attrs)
    _ -> NobodysHome <$> runMessage msg attrs
