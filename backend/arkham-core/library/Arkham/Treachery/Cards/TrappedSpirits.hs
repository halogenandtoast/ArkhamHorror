module Arkham.Treachery.Cards.TrappedSpirits (
  trappedSpirits,
  TrappedSpirits (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Cost
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TrappedSpirits = TrappedSpirits TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

trappedSpirits :: TreacheryCard TrappedSpirits
trappedSpirits = treachery TrappedSpirits Cards.trappedSpirits

instance HasModifiersFor TrappedSpirits where
  getModifiersFor (InvestigatorTarget _) (TrappedSpirits a) = do
    mSource <- getSkillTestSource
    miid <- getSkillTestInvestigator
    modifiers <- case (miid, mSource) of
      (Just iid', Just source) | isSource a source -> do
        mlid <- field InvestigatorLocation iid'
        case mlid of
          Just lid -> do
            isHaunted <- lid <=~> HauntedLocation
            pure $ [CommitCost (ResolveEachHauntedAbility lid) | isHaunted]
          Nothing -> pure []
      _ -> pure []
    pure $ toModifiers a modifiers
  getModifiersFor _ _ = pure []

instance RunMessage TrappedSpirits where
  runMessage msg t@(TrappedSpirits attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ RevelationSkillTest iid (toSource attrs) SkillAgility 3
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n ->
      do
        push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny n 0
        pure t
    _ -> TrappedSpirits <$> runMessage msg attrs
