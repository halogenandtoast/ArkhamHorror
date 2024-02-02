module Arkham.Treachery.Cards.DismalCurse (
  dismalCurse,
  DismalCurse (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DismalCurse = DismalCurse TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

dismalCurse :: TreacheryCard DismalCurse
dismalCurse = treachery DismalCurse Cards.dismalCurse

instance HasModifiersFor DismalCurse where
  getModifiersFor (InvestigatorTarget iid') (DismalCurse a) = do
    mSource <- getSkillTestSource
    mInvestigator <- getSkillTestInvestigator
    case (mSource, mInvestigator) of
      (Just source, Just iid) | iid == iid' && isSource a source -> do
        pure $ toModifiers a [Difficulty 2]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage DismalCurse where
  runMessage msg t@(DismalCurse attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ RevelationSkillTest iid source SkillWillpower 3
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ | isSource attrs source -> do
      horror <- field InvestigatorHorror iid
      sanity <- field InvestigatorSanity iid
      let damage = if horror > sanity * 2 then 4 else 2
      push $ InvestigatorAssignDamage iid source DamageAny damage 0
      pure t
    _ -> DismalCurse <$> runMessage msg attrs
