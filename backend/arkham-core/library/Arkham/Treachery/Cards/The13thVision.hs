module Arkham.Treachery.Cards.The13thVision
  ( the13thVision
  , The13thVision(..)
  )
where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Cost
import Arkham.Criteria
import Arkham.Treachery.Runner

newtype The13thVision = The13thVision TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
    [ restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing $ ActionCost 2 ]

instance RunMessage The13thVision where
  runMessage msg t@(The13thVision attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ AttachTreachery (toId attrs) $ InvestigatorTarget iid
      pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ Discard (toAbilitySource attrs 2) (toTarget attrs)
      pure t
    _ -> The13thVision <$> runMessage msg attrs
