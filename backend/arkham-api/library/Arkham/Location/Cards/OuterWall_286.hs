module Arkham.Location.Cards.OuterWall_286 (outerWall_286, OuterWall_286 (..)) where

import Arkham.Agenda.Sequence (AgendaSide (A, C))
import Arkham.Agenda.Types (Field (AgendaDoom))
import Arkham.GameValue
import Arkham.Helpers.Location (isAt)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype OuterWall_286 = OuterWall_286 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

outerWall_286 :: LocationCard OuterWall_286
outerWall_286 = location OuterWall_286 Cards.outerWall_286 4 (PerPlayer 1)

instance HasModifiersFor OuterWall_286 where
  getModifiersFor (OuterWall_286 a) = whenRevealed a do
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just st -> do
        here <- st.investigator `isAt` a
        if here
          then do
            mDoomA <- traverse (field AgendaDoom) =<< selectOne (AgendaWithSide A)
            mDoomC <- traverse (field AgendaDoom) =<< selectOne (AgendaWithSide C)
            if fromMaybe False ((>) <$> mDoomC <*> mDoomA)
              then modifyEach a (concat $ toList st.committedCards) [DoubleSkillIcons]
              else pure mempty
          else pure mempty

instance RunMessage OuterWall_286 where
  runMessage msg (OuterWall_286 attrs) = OuterWall_286 <$> runMessage msg attrs
