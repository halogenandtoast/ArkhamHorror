module Arkham.Act.Cards.HotOnYourTail (hotOnYourTail) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Investigator.Types (Field (InvestigatorResources))
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait (Trait (Criminal))

newtype HotOnYourTail = HotOnYourTail ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotOnYourTail :: ActCard HotOnYourTail
hotOnYourTail = act (2, A) HotOnYourTail Cards.hotOnYourTail Nothing

instance HasModifiersFor HotOnYourTail where
  getModifiersFor (HotOnYourTail a) = do
    modifySelect a (EnemyWithTrait Criminal) [AddKeyword Keyword.Hunter, HealthModifier 1]

instance HasAbilities HotOnYourTail where
  getAbilities = actAbilities \a ->
    [ restricted a 1 Never $ forced $ InvestigatorResigned #when Anyone
    , onlyOnce $ restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage HotOnYourTail where
  runMessage msg a@(HotOnYourTail attrs) = runQueueT $ case msg of
    -- Ability 1 is keyed on the resignation window, but that window only fires *after*
    -- the investigator has been eliminated -- by which point their resources have been
    -- removed and, for the final investigator, ability 2 has already advanced the act
    -- and resolved the scenario. So we present ability 1 from the pre-elimination
    -- @When (InvestigatorResigned ...)@ message instead, guaranteeing the first ability
    -- resolves before the second one can.
    When (Msg.InvestigatorResigned iid) -> do
      n <- field InvestigatorResources iid
      when (n > 0) $ chooseOneM iid do
        abilityLabeled iid (mkAbility attrs 1 $ forced $ InvestigatorResigned #when Anyone) nothing
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- field InvestigatorResources iid
      when (n > 0) do
        loseResources iid (attrs.ability 1) n
        placeTokens (attrs.ability 1) attrs #resource n
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> HotOnYourTail <$> liftRunMessage msg attrs
