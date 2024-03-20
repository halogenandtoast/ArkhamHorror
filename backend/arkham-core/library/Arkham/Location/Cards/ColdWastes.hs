module Arkham.Location.Cards.ColdWastes (coldWastes, ColdWastes (..)) where

import Arkham.Ability
import Arkham.Helpers.Message qualified as Msg
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Projection

newtype ColdWastes = ColdWastes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldWastes :: LocationCard ColdWastes
coldWastes = location ColdWastes Cards.coldWastes 2 (PerPlayer 1)

instance HasAbilities ColdWastes where
  getAbilities (ColdWastes attrs) =
    extendRevealed
      attrs
      [ restrictedAbility
          attrs
          1
          (DuringSkillTest $ WhileInvestigating $ LocationWithId $ toId attrs)
          $ forced
          $ RevealChaosToken #after You
          $ oneOf [#skull, #cultist, #tablet, #elderthing, #autofail]
      ]

instance RunMessage ColdWastes where
  runMessage msg l@(ColdWastes attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      actionRemainingCount <- field InvestigatorRemainingActions iid
      chooseOrRunOne iid
        $ [Label "Take 1 damage" [Msg.assignDamage iid (attrs.ability 1) 1]]
        <> [Label "Lose 1 action" [LoseActions iid (attrs.ability 1) 1] | actionRemainingCount > 0]
      pure l
    _ -> ColdWastes <$> lift (runMessage msg attrs)
