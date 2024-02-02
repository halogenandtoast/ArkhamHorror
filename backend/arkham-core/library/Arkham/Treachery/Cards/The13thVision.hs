module Arkham.Treachery.Cards.The13thVision (
  the13thVision,
  The13thVision (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype The13thVision = The13thVision TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

the13thVision :: TreacheryCard The13thVision
the13thVision = treachery The13thVision Cards.the13thVision

instance HasModifiersFor The13thVision where
  getModifiersFor SkillTestTarget (The13thVision a) = case treacheryPlacement a of
    TreacheryAttachedTo (InvestigatorTarget iid') -> do
      mSkillTestInvestigator <- getSkillTestInvestigator
      case mSkillTestInvestigator of
        Just iid -> do
          atSameLocation <- iid <=~> colocatedWith iid'
          pure $ toModifiers a [FailTies | atSameLocation]
        _ -> pure []
    _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities The13thVision where
  getAbilities (The13thVision a) =
    [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage The13thVision where
  runMessage msg t@(The13thVision attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ toDiscardBy iid (toAbilitySource attrs 2) attrs
      pure t
    _ -> The13thVision <$> runMessage msg attrs
