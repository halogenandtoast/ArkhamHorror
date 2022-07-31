module Arkham.Treachery.Cards.CurseOfYig
  ( curseOfYig
  , CurseOfYig(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CurseOfYig = CurseOfYig TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfYig :: TreacheryCard CurseOfYig
curseOfYig = treachery CurseOfYig Cards.curseOfYig

instance HasModifiersFor CurseOfYig where
  getModifiersFor _ (InvestigatorTarget iid) (CurseOfYig attrs) =
    pure $ if treacheryOnInvestigator iid attrs
      then toModifiers
        attrs
        [SkillModifier SkillCombat (-1), HealthModifier (-10), AddTrait Serpent]
      else []
  getModifiersFor _ _ _ = pure []

instance RunMessage CurseOfYig where
  runMessage msg t@(CurseOfYig attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    _ -> CurseOfYig <$> runMessage msg attrs
