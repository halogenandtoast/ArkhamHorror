module Arkham.Treachery.Cards.CurseOfYig
  ( curseOfYig
  , CurseOfYig(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.SkillType
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CurseOfYig = CurseOfYig TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfYig :: TreacheryCard CurseOfYig
curseOfYig = treachery CurseOfYig Cards.curseOfYig

instance HasModifiersFor CurseOfYig where
  getModifiersFor (InvestigatorTarget iid) (CurseOfYig attrs) =
    pure $ if treacheryOnInvestigator iid attrs
      then toModifiers
        attrs
        [SkillModifier SkillCombat (-1), HealthModifier (-1), AddTrait Serpent]
      else []
  getModifiersFor _ _ = pure []

instance HasAbilities CurseOfYig where
  getAbilities (CurseOfYig a) = [restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing $ ActionCost 1]

instance RunMessage CurseOfYig where
  runMessage msg t@(CurseOfYig attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      n <- getVengeanceInVictoryDisplay
      push $ beginSkillTest iid attrs iid SkillWillpower (2 + n)
      pure t
    _ -> CurseOfYig <$> runMessage msg attrs
