module Arkham.Treachery.Cards.StrangeSigns (
  strangeSigns,
  StrangeSigns (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Helpers.Investigator
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype StrangeSigns = StrangeSigns TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSigns :: TreacheryCard StrangeSigns
strangeSigns = treachery StrangeSigns Cards.strangeSigns

instance RunMessage StrangeSigns where
  runMessage msg t@(StrangeSigns attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ push (RevelationSkillTest iid source SkillIntellect (Fixed 3))
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          playerCount <- getPlayerCount
          lid <- getJustLocation iid
          let clueCount = if playerCount == 3 || playerCount == 4 then 2 else 1
          t <$ push (PlaceClues (toSource attrs) (LocationTarget lid) clueCount)
    _ -> StrangeSigns <$> runMessage msg attrs
