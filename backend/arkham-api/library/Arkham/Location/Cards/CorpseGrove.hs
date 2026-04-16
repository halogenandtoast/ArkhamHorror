module Arkham.Location.Cards.CorpseGrove (corpseGrove) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CorpseGrove = CorpseGrove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corpseGrove :: LocationCard CorpseGrove
corpseGrove = locationWith CorpseGrove Cards.corpseGrove 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities CorpseGrove where
  getAbilities (CorpseGrove a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced (RevealLocation #after You $ be a)
      , restricted a 2 Here $ forced (SkillTestResult #after You (WhileInvestigating $ be a) #success)
      ]

instance RunMessage CorpseGrove where
  runMessage msg l@(CorpseGrove attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignHorror iid (attrs.ability 2) 1
      pure l
    _ -> CorpseGrove <$> liftRunMessage msg attrs
